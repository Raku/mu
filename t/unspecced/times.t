#!/usr/bin/pugs

use v6;
use Test;

plan 1;

my @start = times();
for (1..1000) {
    1+1;
}
my @end = times();

ok(@end[0] > @start[0], 'something changed in times()');
