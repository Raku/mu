use v6;
use Test;
plan 1;

# P41 (**) A list of Goldbach compositions.
#
# Given a range of integers by its lower and upper limit, print a list of all
# even numbers and their Goldbach composition.
#
# Example:
# * (goldbach-list 9 20)
# 10 = 3 + 7
# 12 = 5 + 7
# 14 = 3 + 11
# 16 = 3 + 13
# 18 = 5 + 13
# 20 = 3 + 17
#
# In most cases, if an even number is written as the sum of two prime numbers,
# one of them is very small. Very rarely, the primes are both bigger than say 50.
# Try to find out how many such cases there are in the range 2..3000.
#
# Example (for a print limit of 50):
# * (goldbach-list 1 2000 50)
# 992 = 73 + 919
# 1382 = 61 + 1321
# 1856 = 67 + 1789
# 1928 = 61 + 1867
#
# Logic and Codes

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

sub goldbachs($from, $to) {
    map { [$_, goldbach $_] }, grep { $_ % 2 == 0 }, $from .. $to
}

is goldbachs(3, 11), ((4, 2, 2), (6, 3, 3), (8, 3, 5), (10, 3, 7)), "yep.";
