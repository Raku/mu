#!/usr/bin/pugs

use v6;
use Test;

plan 2;

=pod 

Basic tests for the sqrt() builtin

=cut

# will these be the same on all machines? or should I truncate them?

is(sqrt(2), 1.4142135623730951, 'got the square root of 2');
is(sqrt(5), 2.23606797749979, 'got the square root of 5');
