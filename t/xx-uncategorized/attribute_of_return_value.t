use v6-alpha;

use Test;

=kwid

Attribute accessors, applied to the return value of a sub, do not work.

This
  class C { has $.a; }  sub f() { C.new() }  f().a
yields
  *** No compatible subroutine found: "&a" where

=cut

plan 4;

class C { has $.a; }
sub f() { C.new(:a(123)) }
sub g() { my C $x .= new(:a(123)); $x }

is(C.new(:a(123)).a, 123, 'C.new().a worked');

my $o = f();
is($o.a, 123, 'my $o = f(); $o.a worked');

is(try { f().a }, 123, 'f().a worked (so the pugsbug is fixed (part 1))');

is(try { g().a }, 123, 'g().a worked (so the pugsbug is fixed (part 2))');

