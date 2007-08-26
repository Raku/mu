#!/usr/bin/env pugs

=begin Problem
The sum of the squares of the first ten natural numbers is,
1² + 2² + ... + 10² = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)² = 55² = 3025

Hence the difference between the sum of the squares of the first ten
natural numbers and the square of the sum is 3025-385 = 2640.

Find the difference between the sum of the squares of the first one
hundred natural numbers and the square of the sum.
=cut

use v6;
use Benchmark;

sub main {
    my $sum_of_squares = sum(map { $_ * $_ }, 1..100);
    my $sum = sum(1..100);
    my $square_of_sums = $sum * $sum;

    say $square_of_sums - $sum_of_squares;
}

my @t = timeit(1, \&main);
say "execution time: @t[0]";
