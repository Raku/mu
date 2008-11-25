#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
A Pythagorean triplet is a set of three natural numbers, a<b<c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
=end Problem

use v6;
use Benchmark;

sub main {

    # if a is 333, then b and c are both > 333, so a+b+c>1000
    for 1..333 -> $a {
        # a < b so start b at a+1
        # if b >= 500, then b+c>1000
        for $a+1 .. 500 -> $b {
            my $c = sqrt($a*$a + $b*$b);
            next unless $a + $b + $c == 1000;
            say $a * $b * $c;
            return;
        }
    }
}

my @t = timeit(1, \&main);
say "execution time: @t[0]"
