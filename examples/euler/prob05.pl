#!/usr/bin/env pugs

=begin Problem
2520 is the smallest number that can be divided by each of the numbers
from 1 to 10 without any remainder.

What is the smallest number that is evenly divisible by all of the
numbers from 1 to 20?
=cut

use v6;
use Benchmark;

sub main {
    for 1..* -> $i {
        if $i % all(1..20) == 0 {
            say $i;
            last;
        }
    }
}

my @t = timeit(1, \&main);
say "execution time: @t[0]";
