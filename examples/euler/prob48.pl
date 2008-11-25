#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
=end Problem

use v6;
use Benchmark;

sub main {
    my $sum = sum(map { $_ ** $_ }, 1..1000);
    say substr($sum, -10, 10);
}

my @t = timeit(1, \&main);
say "execution time: @t[0]"
