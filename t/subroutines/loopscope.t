#!/usr/bin/pugs

use v6;
use Test;

plan 8;

# Implicit $_
for 1, 2 {
    my $inside;
    for 'a' .. 'c' { $inside ~= $_; }
    is($inside, "abc", "lexical scalar properly initialized, round $_");
}

for 1, 2 {
    my @inside;
    for 'a' .. 'c' { push @inside, $_; }
    is(@inside.join(""), "abc", "lexical array properly initialized, round $_");
}

# Explicit $_
for 1, 2 {
    my $inside;
    for 'a' .. 'c' -> $_ { $inside ~= $_; }
    is($inside, "abc", "lexical scalar properly initialized, round $_, explicit \$_");
}

for 1, 2 {
    my @inside;
    for 'a' .. 'c' -> $_ { push @inside, $_; }
    is(@inside.join(""), "abc", "lexical array properly initialized, round $_, explicit \$_");
}
