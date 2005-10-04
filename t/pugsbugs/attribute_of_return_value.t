#!/usr/bin/pugs

use v6;
use Test;

=kwid

Attribute accessors, applied to the return value of a sub, do not work.

This
  class C { has $.a; }  sub f() { C.new() }  f().a
yields
  *** No compatible subroutine found: "&a" where

=cut

plan 3;

class C { has $.a; }
sub f() { C.new(:a(123)) }

is(C.new(:a(123)).a, 123, 'C.new().a worked');

my $o = f();
is($o.a, 123, 'my $o = f(); $o.a worked');

is(eval('f().a'), 123, 'f().a worked (so the pugsbug is fixed)', :todo<bug>);

