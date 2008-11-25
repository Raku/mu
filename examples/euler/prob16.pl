#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
=end Problem

use v6;
use Benchmark;

sub main {
    my $n = 2 ** 1000;
    say sum(split(/./, $n));
}

my @t = timeit(1, \&main);
say "execution time: @t[0]";
