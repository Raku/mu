#!/usr/bin/pugs

use v6;
require Test;

plan 5;

=pod 

Basic tests for the sign() builtin

=cut

is(sign(0), 0, 'got the right sign for 0');
is(sign(-100), -1, 'got the right sign for -100');
is(sign(100), 1, 'got the right sign for 100');
is(sign(1.5), 1, 'got the right sign for 1.5');
is(sign(-1.5), -1, 'got the right sign for -1.5');
