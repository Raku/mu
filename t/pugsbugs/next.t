#!/usr/bin/pugs

use v6;
use Test;

plan 2;

for 1..1 {
    next;
    say "Bail out!";
    say "next doesn't work at all";
    exit();
}

my $foo;
for 1..2 -> $a {
    $foo ~= "A";
    for 1..2 -> $b {
        $foo ~= "B";
        next;             # works on higher level loop, should work on inner
    }
}
is($foo, "ABBABB", "next works on inner loop of 2");

my $bar;
for 1..2 -> $a {
    $bar ~= "A";
    for 1..2 -> $b {
        $bar ~= "B";
        for 1..2 -> $c {
            $bar ~= "C";
            next;         # same thing
        }
    }
}
is($bar, "ABCCBCCABCCBCC", "next works on inner loop of 3");

