use v6;
use Test;
plan 1;

# P40 (**) Goldbach's conjecture.
#
# Goldbach's conjecture says that every positive even number greater than 2 is
# the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
# famous facts in number theory that has not been proved to be correct in the
# general case. It has been numerically confirmed up to very large numbers (much
# larger than we can go with our Prolog system). Write a predicate to find the
# two prime numbers that sum up to a given even integer.
#
# Example:
# * (goldbach 28)
# (5 23)

sub primes($from, $to) {
    my @p = (2);
    for 3..$to -> $x {
        push @p, $x unless grep { $x % $_ == 0 }, 2..ceil sqrt $x;
    }
    grep { $_ >= $from }, @p;
}

sub goldbach($n) {
    my @p = primes(1, $n-1);
    for @p -> $x {
        for @p -> $y {
            return ($x,$y) if $x+$y == $n;
        }
    }
}

is goldbach(28), (5, 23), "Goldbach works.";
