module Parse::Rule::Combinators;

sub empty() {
    Parser.new: parser => sub ($match, &continue) {
        &continue($match);
    }
}

sub delay($code) {
    Parser.new: parser => sub ($match, &continue) {
        $code()($match, &continue);
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

# vim: ft=perl6 :
