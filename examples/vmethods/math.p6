#!/usr/bin/pugs
#
# Please remember to update examples/output/vmethods/math if you change this
# file so its output will change.
#

use v6;

multi sub is_even (Int $value:) {
    $value % 2 == 1;
}

multi sub is_odd (Int $value:) {
    $value % 2 == 0;
}

multi sub is_factor_of (Int $self: Int $num) {
    $num % $self == 0;
}

multi sub is_prime(Int $value:) {
    ?none(2..sqrt($value)).is_factor_of($value);
}

say "5 is " ~ (5.is_even ?? 'even' :: 'odd');
say "8 is " ~ (8.is_odd  ?? 'odd' :: 'even');
say "2 is" ~ (2.is_factor_of(10) ?? ' ' :: ' not ') ~ "factor of 10";
say "3 is" ~ (3.is_factor_of(10) ?? ' ' :: ' not ') ~ "factor of 10";
say "7 is" ~ (7.is_prime ?? ' '::' not ') ~ "a prime number";
say "9 is" ~ (9.is_prime ?? ' '::' not ') ~ "a prime number";
