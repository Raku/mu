use v6;
use Test;
plan 2;

# P39 (*) A list of prime numbers.
#
# Given a range of integers by its lower and upper limit, construct a list of all
# prime numbers in that range.

sub primes($from, $to) {
    my @p = (2);
    for 3..$to -> $x {
        push @p, $x unless grep { $x % $_ == 0 }, 2..ceiling sqrt $x;
    }
    grep { $_ >= $from }, @p;
}

is primes(2,11), (2,3,5,7,11), "a few.";
is primes(16,100), (17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97), "a few more.";

