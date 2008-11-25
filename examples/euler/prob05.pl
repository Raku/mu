#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
2520 is the smallest number that can be divided by each of the numbers
from 1 to 10 without any remainder.

What is the smallest number that is evenly divisible by all of the
numbers from 1 to 20?
=end Problem

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

# this version takes advantage of the fact that the question is really just
# asking for the LCM of 1..20
sub alternate {
    say reduce { lcm($^a, $^b) }, 1..20;
}

sub lcm($a, $b) {
    ($a * $b) / (gcd($a, $b))
}

sub gcd($a, $b) {
    return $a if $b == 0;
    return gcd($b, $a % $b);
}

my @t = timeit(1, \&alternate); # alternate is MUCH faster :)
say "execution time: @t[0]";
