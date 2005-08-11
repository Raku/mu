#!/usr/bin/pugs

use v6;
use Test;

plan 4;

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
