#!/usr/bin/env perl

use strict;
use warnings;

my $delta = shift || 3;
my $start = time();
my $sum = 0;
while (1) {
    last if time() - $start > $delta;
    for (1..100_000_000) {
        if ($sum++ > 100_000_000) {
            $sum = -100_000_000;
        }
    }
}
warn "$0 quits.\n";

