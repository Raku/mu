#!/usr/bin/pugs

use v6;
require Test;

plan 2;

sub foo { return "$?CALLER::POSITION" }
sub bar { gorch("$?CALLER::POSITION") }
sub gorch (Str $str) { return $str }

is(foo(), "t/magicals/caller_pos.t at line 12, column 1", "basic caller position interpolation");

# in the second test case the position is evaluated where gorch is concerned
# that is, bar's call to gorch().

# the variable is interpolated in bar(), and that's where in the call stack
# this should be relevant to.
todo_is(bar(), "t/magicals/caller_pos.t at line 19, column 1", "indirect interpolation (wtf?!)");

