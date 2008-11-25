#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
n! means n x (n - 1) x ... x 3 x 2 x 1

Find the sum of the digits in the number 100!
=end Problem

use v6;
use Benchmark;

proto postfix:<!>($n) {
    return 1 if $n < 2;
    return $n * ($n-1)!
}

sub main {
    say sum split '', 100!
}

my @t = timeit(1, \&main);
say "execution time: @t[0]";
