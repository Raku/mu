module Parse::Rule::Combinators;

use Parse::Rule::Core;

=pod

C<multidex> performs a multidimensional index and copy into a
multidimensional array. C<multidex($a, [1,2,3], 42)> is returns C<$a>
with C<$a[1][2][3]> equal to 42.  The original C<$a> is unchanged.

=cut

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

=pod

C<assertion(&assert)> runs C<&assert>. If the result is true, is continues. If
it is false, it backtracks.

=cut

sub assertion ($assert) is export {
    Parser.new: parse => sub ($match, &continue) {
        if $assert($match) {
            &continue($match);
        }
        else {
            $match.backtrack()();
        }
    }
}

=pod

C<empty> matches nothing, successfully.

=cut

sub empty () is export {
    Parser.new: parse => sub ($match, &continue) {
        &continue($match);
    }
}

=pod

C<concat($a, $b)> first tries to match C<$a>, then tries C<$b> where
C<$a> left off.  In case C<$b> fails, it backtracks into C<$a> (just
like juxtaposition in regexes).

=cut

sub concat (Parser $a, Parser $b) is export {
    Parser.new: parse => sub ($match, &continue) {
        $a.parse()($match, -> $m { $b.parse()($m, &continue) });
    }
}

=pod

C<alternate($a, $b)> tries C<$a>, and tries C<$b> if C<$a> fails.

=cut

sub alternate (Parser $a, Parser $b) is export {
    Parser.new: parse => sub ($match, &continue) {
        $a.parse()($match.clone(
            backtrack => -> {
                $b.parse()($match, &continue);
            }
        ),
        &continue);
    }
}

=pod

C<quantify($p, $low, $high)> tries to match C<$p> at least C<$low>
times, and at most C<$high> times.  If C<$low> is not given, it
defaults to 0.  If C<$high> is not given, it defaults to infinity.  So
C</a*/> can be written C<quantify(literal("a"))>.

Captures within quantified expressions are "parallelized".  That is, /[
foo (bar) ]*/ will put an array in $1, and /[ [ foo (bar) ]* ]*/ will
put an array of arrays in $1.  This is done by setting
C<$match.multidex> to the index where captures should put themselves.
In the former example, when C<(bar)> runs, C<multidex> will be, say,
C<[3]> (on the fourth match of the group).  In the latter, it will be,
say, C<[2,4]> (on the third match of the outer group, and the fifth
match of the inner group).

C<quantify> also takes a C<:minimal> argument, which, if present, tells
the combinator to succeed as soon as it has matched C<$low> times, and
backtrack by matching more.

=cut

sub quantify (Parser $p, $low? = 0, $high? = Inf, :$minimal = 0) is export {
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
                my $multidex = [ *$match.match.multidex ];
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
            # In here, we are in the specified number of times.  
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

    Parser.new: parse => sub ($match, &continue) {
        my $mdex = $match.match.multidex;
        # Put another dimension on the multidex and match the recursive match_n
        # combinator.
        match_n(0, $match.clone(
                      match => $match.match.clone(multidex => [ *$mdex, 0 ])),
                -> $m { 
                    &continue($m.clone(match => $m.match.clone(multidex => $mdex)));
                });
    }
}

=pod

C<optional($p)> is pretty much the same as C<quantify($p,0,1)>,  but it
has its own combinator because it stores captures differently.  Instead
of creating another level of arrays in the captures, it simply doesn't
put anything there if it didn't match.

C<optional> also takes a C<:minimal> argument, which does the same thing
as it does above in C<quantify>.

=cut

sub optional ($p, :$minimal = 0) is export {
    if $minimal {
        Parser.new: parse => sub ($match, &continue) {
            &continue($match.clone(
                backtrack => -> { $p.parse()($match, &continue) }));
        }
    }
    else {
        Parser.new: parse => sub ($match, &continue) {
            $p.parse()($match.clone(
                backtrack => -> { &continue($match) }),
                &continue);
        }
    }
}

=pod

C<capture($p, :num($num), :name($name))> will capture whatever C<$p> matched
into C<$match.capture_num[$num]> and C<$match.capture_name{$name}>.  C<$p> will
be given a fresh, empty C<$match.match> to fill (this introduces a new scope).

=cut

sub capture (Parser $p, :$num, :$name) is export {
    $num // $name // die 'capture must be given either a :num or :name argument';
    Parser.new: parse => sub ($match, &continue) {
        $p.parse()($match.clone(match => Match.new),
                   -> $m {
                        # XXX. Eeeeeeyuck!  This is totally awkward.
                        my $subobj = $m.match.clone(
                            start => $match.pos,
                            end => $m.pos,
                        );
                        my $mmat = $match.match;
                        my $newnump = $mmat.capture_num;
                        my $newnamep = $mmat.capture_name;
                        if defined $num {
                            my $newnum = defined $num
                                            ?? multidex($mmat.capture_num[$num], $match.match.multidex, $subobj) 
                                            !! $mmat.capture_num[$num];
                            $newnump = [ @$newnump ];
                            $newnump[$num] = $newnum;
                        }
                        if defined $name {
                            my $newname = defined $name
                                            ?? multidex($mmat.capture_name{$name}, $match.match.multidex, $subobj)
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
    }
}

=pod

C<mark($p, $name)> simply executes C<$p>, while setting up a
backtracking mark named C<$name>.  The name is scoped to C<$p>.  That
is, once the match succeeds beyond C<$p>, the mark is no longer valid.
This gives the impression of lexically scoped marks (but it may also
limit the engine semantically, we shall see).  See C<commit>.

=cut

sub mark (Parser $p, Str $name) is export {
    Parser.new: parse => sub ($match, &continue) {
        my $marks = $match.marks;
        my $nmarks = { %$marks, $name => $match.backtrack };
        $p.parse()($match.clone( 
                       marks => { %$marks, $name => $match.backtrack }),
                    -> $m { &continue($m.clone(marks => $marks)) });
    }
}

=pod

C<commit($name)> will always succeed.  When backtracked over, it skips
to the C<mark> (see above) named C<$name>.  The current implementation
also forgets all state between the C<mark> and the C<commit> when the
commit succeeds, so this can be used to help matches be more
memory-efficient.

=cut

sub commit (Str $name) is export {
    Parser.new: parse => sub ($match, &continue) {
        my $marks = $match.marks;
        $match.marks{$name} // die "No mark with name '$name' in soope";
        &continue($match.clone(
                    backtrack => $match.marks(){$name}));
    }
}

# vim: ft=perl6 :
