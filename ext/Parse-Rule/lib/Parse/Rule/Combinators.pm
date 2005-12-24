module Parse::Rule::Combinators;

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

sub empty () is export {
    Parser.new: parse => sub ($match, &continue) {
        &continue($match);
    }
}

sub concat (Parser $a, Parser $b) is export {
    Parser.new: parse => sub ($match, &continue) {
        $a.parse()($match, -> $m { $b.parse()($m, &continue) });
    }
}

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

sub quantify (Parser $p, $low? = 0, $high? = Inf) is export {
    my sub match_n ($n, $match, &continue) {
        my $multidex = [ *$match.match.multidex ];
        # what I would give for a native linked list
        $multidex[-1]++;
        my $new_continue = -> $m { 
            my $mp = $m.pos;  my $map = $match.pos;
            if $mp.pos == $map.pos {   # XXX Not medium-independent!
                # inside a quantifier, making no progress is the same as failing
                #  (to avoid infinite loops)
                $m.backtrack()();
            }
            else {
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
            $p.parse()($match.clone(backtrack => -> { &continue($match) }),
                       $new_continue);
        }
    }

    Parser.new: parse => sub ($match, &continue) {
        my $mdex = $match.match.multidex;
        match_n(0, $match.clone(
                      match => $match.match.clone(multidex => [ *$mdex, 0 ])),
                -> $m { 
                    &continue($m.clone(match => $m.match.clone(multidex => $mdex)));
                });
    }
}

sub capture (Parser $p, :$num, :$name) is export {
    $num // $name // die 'capture must be given either a :num or :name argument';
    Parser.new: parse => sub ($match, &continue) {
        $p.parse()($match.clone(match => Match.new),
                   -> $m {
                        # Eeeeeeyuck!  This is totally awkward.
                        my $subobj = $m.match.clone(
                            start => $match.pos,
                            end => $m.pos,
                        );
                        my $mmat = $match.match;
                        my $newnump = $mmat.match_num;
                        my $newnamep = $mmat.match_name;
                        if defined $num {
                            my $newnum = defined $num
                                            ?? multidex($mmat.match_num[$num], $match.match.multidex, $subobj) 
                                            !! $mmat.match_num[$num];
                            $newnump = [ @$newnump ];
                            $newnump[$num] = $newnum;
                        }
                        if defined $name {
                            my $newname = defined $name
                                            ?? multidex($mmat.match_name{$name}, $match.match.multidex, $subobj)
                                            !! $mmat.match_name{$name};
                            $newnamep = { %$newnamep };
                            $newnamep{$name} = $newname;
                        }
                        my $obj = $mmat.clone(
                            match_num => $newnump,
                            match_name => $newnamep,
                        );

                        &continue($m.clone(match => $obj));
                    });          
    }
}

sub mark (Parser $p, Str $name) is export {
    Parser.new: parse => sub ($match, &continue) {
        my $marks = $match.marks;
        my $nmarks = { %$marks, $name => $match.backtrack };
        $p.parse()($match.clone( 
                       marks => { %$marks, $name => $match.backtrack }),
                    -> $m { &continue($m.clone(marks => $marks)) });
    }
}

sub cut (Str $name) is export {
    Parser.new: parse => sub ($match, &continue) {
        my $marks = $match.marks;
        $match.marks{$name} // die "No mark with name '$name' in soope";
        &continue($match.clone(
                    backtrack => $match.marks(){$name}));
    }
}

# vim: ft=perl6 :
