# Print out text in a little box like so:
# +------+
# | Pugs |
# +------+

use v6;

sub longest_line(@lines) {
    @lines.max:{ $^a.codes <=> $^b.codes }.codes;
}

sub lfill(Str $s, Num $n) {
    return $s if $s.codes >= $n;
    return $s ~ ' ' x ($n - $s.codes);
}

sub box(@lines, $width=0) {
    my $w = max($width, longest_line(@lines));
    say "+-{ '-' x $w }-+";
    for @lines {
        say "| " ~ lfill($^line, $w) ~ " |";
    }
    say "+-{ '-' x $w }-+";
}

box(["Perl 6",
     "Pugs",
     "Lambda Camels",
     "λ"]);

box(["Perl 6",
     "Pugs",
     "Lambda Camels",
     "λ"], 40);

box(["Perl 6",
     "Pugs",
     "Lambda Camels",
     "λ"], 4);
