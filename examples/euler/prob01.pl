#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
If we list all the natural numbers below 10 that are multiples of 3 or
5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
=end Problem

use v6;
use Benchmark; #<timeit>;

sub main {
    my @multiples = grep { $^a % 3 == 0 || $^a % 5 == 0 }, 1..^1000;
    say [+](@multiples);
}
my @t = timeit(1, &main);
say "execution time: @t[0]"
