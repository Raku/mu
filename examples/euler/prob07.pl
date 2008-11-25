#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can
see that the 6th prime is 13.

What is the 10001st prime number?
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
    my $prime;
    my $count = 0;

    for 2..* -> $i {
        $count++ if is_prime($i);
        if $count == 10001 {
            say $i;
            last;
        }
    }
}

my @t = timeit(1, \&main);
say "execution time: @t[0]";
