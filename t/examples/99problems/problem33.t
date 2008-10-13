use v6;
use FindBin;

# P33 (*) Determine whether two positive integer numbers are coprime.
# 
# Two numbers are coprime if their greatest common divisor equals 1.
# Example:
# * (coprime 35 64)
# T
@INC.push($FindBin::Bin);
require "problem32.t";

sub coprime(Int $a, Int $b) { gcd($a,$b) == 1}

unless caller() {
    use Test;
    plan 3;
    ok  coprime(35,64), "We should be able to tell that 35 and 64 are coprime";
    ok  coprime(64,35), ".. and viceversa";
    ok !coprime(13,39), ".. but 13 and 39 are not";
}
