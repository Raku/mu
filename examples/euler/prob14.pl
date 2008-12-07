#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
The following iterative sequence is defined for the set of positive
integers:

n -> n/2 (n is even)
n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following
sequence:
13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

It can be seen that this sequence (starting at 13 and finishing at 1)
contains 10 terms. Although it has not been proved yet (Collatz
Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one
million.
=end Problem

use v6;
use Benchmark;

sub do_chain(%h, $n is copy) {
    my $x = 0;

    while $n != 1 {
        if exists(%h, $n) {
            $x += %h{$n};
            last;
        }

        if $n % 2 == 0 {
            $n = $n / 2;
        }
        else {
            $n = 3 * $n + 1;
        }

        $x++;
    }

    return $x;
}

sub main {
    my @longest = (0, 0);
    my %h;

    for 1..999_999 -> $i {
        my $length = do_chain(%h, $i);
        %h{$i} = $length;
        if $length > @longest[1] {
            @longest = ($i, $length);
        }
    }
    say @longest;
}

my @t = timeit(1, \&main);
say "execution time: @t[0]";
