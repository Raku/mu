#!/usr/bin/pugs

use v6;
require Test;

plan 2;

=pod 

Basic tests for the rand() builtin

=cut

ok(rand() > 0, 'random numbers are greater than 0');
ok(rand() < 1, 'random numbers are less than 1');