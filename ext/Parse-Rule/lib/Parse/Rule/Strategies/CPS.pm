role Parse::Rule::Strategies::CPS;

use Parse::Rule::Strategy;
does Parse::Rule::Strategy;

=pod

C<Result> is the match state that is passed around.  It essentially
forms a linked list through the rule as it is executed, but the linked
list is done in code (i.e. with closures).  It has the following
members:

    $.backtrack - Parsers tail-call this if they fail (see below)
    $.marks     - A hash that stores backtrack closures to various.
                  points earlier in the match.  This is used to
                  implement backtracking controls.  a*: ...
    $.pos       - The current position of the match (also holds the
                  string that is being matched against).
    $.match     - State that is scope-local.  A new match object gets
                  allocated when you enter a new rule or parenthesized
                  group.

=cut

class Parse::Rule::Strategies::CPS::Result {
    has $.backtrack;
    has $.marks;
    has $.pos;
    has $.match;

    submethod BUILD () {
        $.marks //= {};
    }
}

=pod

A C<Parser> is simply an object wrapper around a closure.  The closure,
C<$.parse>, takes two parameters:  a C<Result> object and a continuation
(which takes in turn a C<Result> object as a parameter).  If the parser
succeeds, it tail-calls the continuation (i.e. returns whatever the
continuation returns) with an updated C<Result> object.  If the parser
fails, it tail-calls $result.backtrack()().  Backtracking parsers such
as quantifiers are implemented by passing a C<Result> object that has a
$.backtrack closure that calls back into the parser.

=cut

class Parse::Rule::Strategies::CPS::Parser {
    does Parse::Rule::Core::Parser;
    does Parse::Rule::Core::Rule;

    method compile() { $?SELF }   # no compilation phase
    method run($input, $match) {
        $.parse(
            # XXX this class should be virtual
            Parse::Rule::Strategies::CPS::Result.new(
                backtrack => -> { return },
                pos       => $input,
                match     => $match),
            -> $m { ($m.match.clone(
                        from => $input,
                        to   => $m.pos), 
                     $m.backtrack) });
    }

    has $.parse;
}

=pod

C<$.Parser> and C<$.Result> are virtual classes within the strategy.  They're
done like this until pugs gets support for virtual classes.

=cut

has $.Parser;
has $.Result;

submethod BUILD () {
    $.Result = Parse::Rule::Strategies::CPS::Result;
    $.Parser = Parse::Rule::Strategies::CPS::Parser;
}



my sub multidex ($container, $multidex, $value) {
    if +$multidex == 0 {
        $value;
    }
    else {
        my $ret = $container ?? [ *$container ] !! [ ];
        my $idx = $multidex[0];
        $ret[$idx] = multidex($ret[$idx], $multidex[1..^$multidex], $value);
        $ret;
    }
}

method assertion ($assert) {
    $.Parser.new(parse => sub ($match, &continue) {
        if $assert($match) {
            &continue($match);
        }
        else {
            $match.backtrack()();
        }
    });
}

method empty () {
    $.Parser.new(parse => sub ($match, &continue) {
        &continue($match);
    });
}

method concat ($a, $b) {
    $.Parser.new(parse => sub ($match, &continue) {
        $a.parse()($match, -> $m { $b.parse()($m, &continue) });
    });
}

method alternate ($a, $b) {
    $.Parser.new(parse => sub ($match, &continue) {
        $a.parse()($match.clone(
            backtrack => -> {
                $b.parse()($match, &continue);
            }
        ),
        &continue);
    });
}

=pod

Captures within quantified expressions are "parallelized".  That is, /[ foo
(bar) ]*/ will put an array in $1, and /[ [ foo (bar) ]* ]*/ will put an array
of arrays in $1.  This is done by setting C<$match.multidex> to the index where
captures should put themselves.  In the former example, when C<(bar)> runs,
C<multidex> will be, say, C<[3]> (on the fourth match of the group).  In the
latter, it will be, say, C<[2,4]> (on the third match of the outer group, and
the fifth match of the inner group).  XXX wrong; they should be "flat".

=cut

method quantify ($p, $low? = 0, $high? = Inf, :$minimal = 0) {
    my sub match_n ($n, $match, &continue) {
        my $new_continue = -> $m { 
            my $mp = $m.pos;  my $map = $match.pos;
            if $mp.pos == $map.pos {   # XXX Not medium-independent!
                # inside a quantifier, making no progress is the same as failing
                #  (to avoid infinite loops)
                $m.backtrack()();
            }
            else {
                # Each time around the match, we increment the inner multidex index.
                my $matchy = $match.match;
                my $multidex = [ *$matchy.multidex ];
                $multidex[-1]++;
                match_n($n+1, $m.clone(match => $m.match.clone(multidex => $multidex)), &continue);
            }
        };
        if $n < $low {
            $p.parse()($match, $new_continue);
        }
        elsif $n > $high {
            $match.backtrack()();
        }
        else {
            # In here, we are within the specified number of times.  
            if $minimal {
                &continue($match.clone(backtrack => -> { 
                                         $p.parse()($match, $new_continue) }))
            }
            else {  # greedy
                $p.parse()($match.clone(backtrack => -> { &continue($match) }),
                           $new_continue);
            }
        }
    }

    $.Parser.new(parse => sub ($match, &continue) {
        my $matchy = $match.match;    # pugs pisses me off
        my $mdex = $matchy.multidex;
        # Put another dimension on the multidex and match the recursive match_n
        # combinator.
        match_n(0, $match.clone(
                      match => $match.match.clone(multidex => [ *$mdex, 0 ])),
                -> $m { 
                    &continue($m.clone(match => $m.match.clone(multidex => $mdex)));
                });
    });
}

method optional ($p, :$minimal = 0) {
    if $minimal {
        $.Parser.new(parse => sub ($match, &continue) {
            &continue($match.clone(
                backtrack => -> { $p.parse()($match, &continue) }));
        });
    }
    else {
        $.Parser.new(parse => sub ($match, &continue) {
            $p.parse()($match.clone(
                backtrack => -> { &continue($match) }),
                &continue);
        });
    }
}

method capture ($p, Int :$num, Str :$name) is export {
    $.Parser.new(parse => sub ($match, &continue) {
        $p.parse()($match.clone(match => Parse::Rule::Core::Match.new),
                   -> $m {
                        # XXX. Eeeeeeyuck!  This is totally awkward.
                        my $subobj = $m.match.clone(
                            from => $match.pos,
                            to => $m.pos,
                        );
                        my $mmat = $match.match;
                        my $newnump = $mmat.capture_num;
                        my $newnamep = $mmat.capture_name;
                        if defined $num {
                            my $newnum = defined $num
                                            ?? multidex($mmat.capture_num[$num], $mmat.multidex, $subobj) 
                                            !! $mmat.capture_num[$num];
                            $newnump = [ @$newnump ];
                            $newnump[$num] = $newnum;
                        }
                        if defined $name {
                            my $newname = defined $name
                                            ?? multidex($mmat.capture_name{$name}, $mmat.multidex, $subobj)
                                            !! $mmat.capture_name{$name};
                            $newnamep = { %$newnamep };
                            $newnamep{$name} = $newname;
                        }
                        my $obj = $mmat.clone(
                            capture_num => $newnump,
                            capture_name => $newnamep,
                        );

                        &continue($m.clone(match => $obj));
                    });          
    });
}

method mark ($p, Str $name) {
    $.Parser.new(parse => sub ($match, &continue) {
        my $marks = $match.marks;
        $p.parse()($match.clone( 
                       marks => { %$marks, $name => $match.backtrack }),
                    -> $m { &continue($m.clone(marks => $marks)) });
    });
}

method commit (Str $name) {
    $.Parser.new(parse => sub ($match, &continue) {
        my $marks = $match.marks;
        $marks{$name} // die "No mark with name '$name' in scope";
        &continue($match.clone(
                    backtrack => $marks{$name}));
    });
}

method subrule (Code $code) {
    $.Parser.new(parse => sub ($match, &continue) {
        my $parser = $code($match);
        $parser.parse()($match, &continue);
    });
}

# vim: ft=perl6 :
