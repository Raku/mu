use v6-alpha;
use Test;
plan 4;

# P32 (**) Determine the greatest common divisor of two positive integer numbers.
# 
# Use Euclid's algorithm.
# Example:
# * (gcd 36 63)
# 9

# Makes sense to declare types since gcd makes sense only for Ints.
# Yet, it should be possible to define it even for commutative rings
# other than Integers, so we use a multi sub.
#
# We don't need to rewrite this in iterative form since Perl6 imposes 
# tail call optimization.

multi sub gcd(Int $a, Int $b){
    return $a if $b == 0;
    return gcd($b,$a % $b);
}

is gcd(36,63), 9, "We should be able to find the gcd of 36 and 63";
is gcd(63,36), 9, ".. and viceversa";
is gcd(0,5)  , 5, '.. and that gcd(0,$x) is $x';
is gcd(0,0)  , 0, '.. even when $x is 0';
