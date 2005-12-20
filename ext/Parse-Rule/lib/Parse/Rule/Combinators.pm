module Parse::Rule::Combinators;

sub concat(Parser $a, Parser $b) {
    Parser.new: parser => sub ($match, &continue) {
        $a.parse()($match, -> $m { $b.parse($m, &continue) });
    }
}

sub alternate(Parser $a, Parser $b) {
    Parser.new: parser => sub ($match, &continue) {
        $a.parse()($match.clone(
            backtrack => -> {
                $b.parse()($match, &continue);
            }
        ));
    }
}

sub quantify(Parser $a, Int $min? = 0, Int $max?) {
    my $unbounded = not defined $max;
    Parser.new: parser => sub ($match, &continue) {
        
    }
}

# vim: ft=perl6 :
