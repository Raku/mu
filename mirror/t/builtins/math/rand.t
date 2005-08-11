#!/usr/bin/pugs

use v6;
use Test;

plan 2 + 2*10;

=pod 

Basic tests for the rand() builtin

=cut

# L<S29/"Math::Basic" /rand/>

ok(rand() >= 0, 'rand() returns numbers greater than or equal to 0');
ok(rand() < 1, 'rand() returns numbers less than 1');

for 1 .. 10 {
  ok rand(10) >=  0, "rand(10) always returns numbers greater than or equal to 0 ($_)";
  ok rand(10)  < 10, "rand(10) always returns numbers less than 10 ($_)";
}
