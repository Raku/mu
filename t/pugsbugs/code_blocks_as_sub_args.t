#!/usr/bin/pugs

use v6;
require Test;

plan 2;

=pod

Test a bug where sub args of type Sub do not get handled correctly.

=cut

sub foo (Sub $code, Str $a, Str $b) { return $a.ref }

eval_is('foo(-> { die "test" }, "a", "b")', "Str", 'this will die with a "No compatible subroutine found: &foo"');

sub foo2 (Sub $code, Str $a, Str ?$b) { return $a.ref }

eval_is('foo2(-> { die "test" }, "a", "b")', "Str", 'this should return "Str"');
