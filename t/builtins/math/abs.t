#!/usr/bin/pugs

use v6;
require Test;

plan 4;

=pod 

Basic tests for the abs() builtin

=cut

is(abs(0), 0, 'got the right absolute value for 0');
is(abs(1), 1, 'got the right absolute value for 1');
is(abs(-1), 1, 'got the right absolute value for -1');
is(abs(-50), 50, 'got the right absolute value for -50');
