module Parse::Rule::Combinators;

my sub multidex($container is rw, $multidex) is rw {
    if +$multidex == 0 {
        $container;
    }
    else {
        multidex($container[$multidex[0]], $multidex[1..^$multidex]);
    }
}

sub empty() is export {
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
        my $multidex = $match.match.multidex.clone;
        # what I would give for a native linked list
        $multidex[-1]++;
        if $n < $low {
            $p.parse()($match, -> $m { 
                match_n($n+1, 
                        $match.clone(match => $match.match.clone(multidex => $multidex)), 
                        &continue);
            });
        }
        elsif $n > $high {
            $match.backtrack()();
        }
        else {
            $p.parse()($match.clone(backtrack => &continue),
                       -> $m { match_n($n+1, $m, &continue) });
        }
    }

    Parser.new: parse => sub ($match, &continue) {
        my $mdex = $match.match.multidex;
        match_n(0, [], 
                $match.clone(
                    match => $match.match.clone(multidex => [ *$mdex, 0 ])),
                -> $m { 
                    my $mdex = $m.value.multidex;
                    &continue($m.clone(match => $m.match.clone(multidex => $mdex)));
                });
    }
}

sub capture (Parser $p, :$num, :$name) is export {
    $num // $name // die 'capture must be given either a :num or :name argument';
    Parser.new: parse => sub ($match, &continue) {
        $p.parse()($match.clone(match => Match.new),
                   -> $m {
                        my $obj = $match.match.clone;
                        multidex($obj.match_num, $obj.multidex) = 
                            $m.match.clone(
                                start => $match.pos,
                                end   => $m.pos,
                            );
                            
                        &continue($m.clone(match => $obj));
                    });          
    }
}

# vim: ft=perl6 :
