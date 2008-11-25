#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
The sum of the squares of the first ten natural numbers is,
1² + 2² + ... + 10² = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)² = 55² = 3025

Hence the difference between the sum of the squares of the first ten
natural numbers and the square of the sum is 3025-385 = 2640.

Find the difference between the sum of the squares of the first one
hundred natural numbers and the square of the sum.
=end Problem

use v6;
use Benchmark;

sub main {
    say ([+] 1..100) ** 2 - [+] map { $_ **2 }, 1..100;
}

my @t = timeit(1, \&main);
say "execution time: @t[0]";
