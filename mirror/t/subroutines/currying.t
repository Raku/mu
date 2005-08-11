#!/usr/bin/pugs

use v6;
use Test;

=kwid

Tests curried subs as defined by L<S06/Currying>

= TODO

* assuming on a use statement

=cut

plan 8;

sub foo (+$x, +$y, +$z = 'd') {
	"x=$x y=$y z=$z";
}

is(foo(1, 2), "x=1 y=2 z=d", "uncurried sub has good output");
is(foo("x" => 1, "y" => 2), "x=1 y=2 z=d", "uncurried sub with pair notation");

is((&foo.assuming("y" => 2))("x" => 1), foo(1, 2), "curried sub with named params");

is((&foo.assuming("y" => 2))(1), foo(1, 2), "curried sub, mixed notation");

is((&foo.assuming("x" => 1))(2), foo(1, 2), "same thing, but the other way around");

ok(!(try { &foo.assuming(1) }), "can't curry without named params",:todo); # L<S06/Currying /takes a series of named arguments/> 

ok(!(try { &foo.assuming("f" => 3) }), "can't curry nonexistent named param",:todo); # L<S06/Currying /whose names must match parameters of the subroutine itself/> 

# L<S06/"Currying" /The result of a use statement/>
(eval('use t::packages::Test') // {}).assuming(arg1 => "foo");
is try { dummy_sub_with_params(arg2 => "bar") }, "[foo] [bar]",
  "(use ...).assuming works", :todo<feature>;
