#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 317584931803?
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

sub divide($n) {
    my $i = 2;
    while $i <= $n {
        return ($i, $n / $i) if $n % $i == 0;
        $i += 1;
    }
}

sub prime_factors($n) {
    if is_prime($n) {
        ($n);
    }
    else {
        my ($x, $y) = divide($n);
        (prime_factors($x), prime_factors($y));
    }
}

my @t = timeit(1, -> { say max(prime_factors(317584931803)) });
say "execution time: @t[0]";
