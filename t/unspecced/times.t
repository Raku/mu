#!/usr/bin/pugs

use v6;
use Test;

plan 1;

sub n_le(Int $a, Int $b) { ?($a <= $b) }; # how stupid is this?

my @start = times();
for (1..1000) {
    1+1;
}
my @end = times();
ok(@end[0] > @start[0], 'something changed in times()');

cmp_ok(@end[0] - @start[0], &n_le, 10, 'sensible time spent');
