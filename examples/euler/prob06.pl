#!/usr/bin/env pugs

use v6;

my $sum_of_squares = sum(map { $_ * $_ }, 1..100);
my $sum = sum(1..100);
my $square_of_sums = $sum * $sum;

say $square_of_sums - $sum_of_squares;
