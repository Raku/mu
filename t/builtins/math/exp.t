#!/usr/bin/pugs

use v6;
use Test;

plan 1;

=pod 

Basic tests for the exp() builtin

=cut

sub is_approx (Num $is, Num $expected, Str $descr) {
  ok abs($is - $expected) <= 0.00001, $descr;
}

is_approx(exp(5), 148.4131591025766, 'got the exponent of 5');
