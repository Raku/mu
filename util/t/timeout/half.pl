#!/usr/bin/env perl

use strict;
use warnings;
use Time::HiRes 'sleep';

my $delta = shift;
my $start = time();
my $sum = 0;
while (1) {
    last if time() - $start > $delta;
    if ($sum++ > 100_000_000) {
        $sum = -100_000_000;
    }
    sleep(0.01);
}
warn "$0 quits.\n";

