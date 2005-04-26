#!/usr/bin/pugs

use v6;
use Test;

plan 4;

=pod

Basic tests for the log() and log10() builtins

=cut

# will this be the same on all machines? or should I truncate it?

#test data was originally 1.6094379124341
is(log(5), 1.6094379124341003, 'got the log of 5');
is(log(0.1), -2.3025850929940455, 'got the log of 0.1');

is(log10(5), 0.6989700043360187, 'got the log10 of 5');
is(log10(0.1), -0.9999999999999998, 'got the log10 of 0.1');

# please add tests for complex numbers
