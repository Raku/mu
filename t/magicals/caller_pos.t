#!/usr/bin/pugs

use v6;
require Test;

plan 3;

sub foo { return "$?CALLER::POSITION" }
sub bar { gorch("$?CALLER::POSITION") }
sub gorch (Str $str) { return $str }

is(foo(), "$?FILE at line 12, column 1", "basic caller position interpolation");

# in the second test case the position is evaluated where gorch is concerned
# that is, bar's call to gorch().

# the variable is interpolated in bar(), and that's where in the call stack
# this should be relevant to.
is(bar(), "$?FILE at line 19, column 1", "indirect interpolation (wtf?!)");

sub inner { return join("\n", eval '$?CALLER::CALLER::POSITION', eval '$?CALLER::POSITION') }
sub outer { inner() }

todo_is(outer(), "$?FILE at line 24, column 1\n$?FILE at line 22, column 13", "caller::caller notation works");

