#!/usr/bin/pugs

use v6;
use Test;

=pod

This test checks the semantics of return().

=cut

plan 1;

# test case by Bryan Donlan

is( try { sub foo { while (1) { return 24; } return 42; } foo() }, 24, 'return in while');

