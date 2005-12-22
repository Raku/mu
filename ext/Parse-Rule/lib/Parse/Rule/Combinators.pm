module Parse::Rule::Combinators;

sub empty() {
    Parser.new: parser => sub ($match, &continue) {
        &continue($match);
    }
}

sub concat (Parser $a, Parser $b) {
    Parser.new: parser => sub ($match, &continue) {
        $a.parse()($match, -> $m { $b.parse($m, &continue) });
    }
}

sub alternate (Parser $a, Parser $b) {
    Parser.new: parser => sub ($match, &continue) {
        $a.parse()($match.clone(
            backtrack => -> {
                $b.parse()($match, &continue);
            }
        ));
    }
}

sub quantify (Parser $a, $low? = 0, $high? = Inf) {
    my sub match_n ($n, $prevm, $match, &continue) {
        if $n < $low {
            $a.parse()($match, -> $m { 
                match_n($n+1, [ *$prevm, $m.value ],
                        $match.clone(pos => $m.pos), &continue);
            });
        }
        elsif $n > $high {
            $match.backtrack()();
        }
        else {
            $a.parse()($match.clone(
                backtrack => -> { 
                    &continue(Match::combine($prevm, $match.pos));
                }), 
                -> $m { match_n($n+1, [ *$prevm, $m.value ], $m, &continue) });
        }
    }

    Parser.new: parse => sub ($match, &continue) {
        match_n(0, [], $match, &continue);
    }
}

# vim: ft=perl6 :
