#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below one million.
=end Problem

use v6;
use Benchmark;

sub is_prime($n) {
    my @primes = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
        47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109,
        113);

    return True  if $n == any(@primes);
    return False if $n % any(@primes) == 0;
    return False if $n % any(2..sqrt($n)) == 0;
    return True;
}

sub main {
    my @primes = grep { is_prime($_) }, 2..999_999;
    say sum(@primes);
}

my @t = timeit(1, \&main);
say "execution time: @t[0]";
