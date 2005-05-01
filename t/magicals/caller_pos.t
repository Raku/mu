#!/usr/bin/pugs

use v6;
use Test;

plan 8;

is($?POSITION, "$?FILE line 8, column 4-14", 'plain old $?POSITION');

sub foo { return "$?CALLER::POSITION" }
sub bar { gorch("$?CALLER::POSITION") }
sub gorch (Str $str) { return $str }

is(foo(), "$?FILE line 14, column 4-9", "basic caller position interpolation");

# in the second test case the position is evaluated where gorch is concerned
# this, bar's call to gorch().

# the variable is interpolated in bar(), and that's where in the call stack
# this should be relevant to.
is(bar(), "$?FILE line 21, column 4-9", "indirect interpolation (wtf?!)");

sub inner { return join("\n", $?CALLER::CALLER::CALLER::POSITION, $?CALLER::CALLER::POSITION, $?CALLER::POSITION) }
sub outer { inner() }
sub very  { outer() }

is(very(), "$?FILE line 27, column 4-10\n$?FILE line 25, column 13-21\n$?FILE line 24, column 13-21", "caller::caller notation works");

ok(!(eval 'outer(); 1'), "can't look beyond top level caller");

sub inner2 {
#line 32
  return join("\n", $?CALLER::CALLER::CALLER::POSITION, $?CALLER::CALLER::POSITION, $?CALLER::POSITION)
}
sub outer2 { 
#line 36
  inner2()
}
sub very2 {
#line 40
  outer2() 
}

# Are the lines/columns right here?
is(very2(), "$?FILE line 44, column 4-11\n$?FILE line 40, column 3 - line 41, column 1\n$?FILE line 36, column 3 - line 37, column 1", "caller::caller notation works across multiple lines");

sub line_set { return "$?CALLER::POSITION" };

#line 666
sub get_line { return line_set };
is(get_line, "$?FILE line 666, column 23-32", "#line works for caller::caller notation");

sub line_set2 { return "$?CALLER::POSITION" }

#line 666
sub get_line2 { return line_set2 };
is(get_line2, "$?FILE line 666, column 24-34", "#line works for caller::caller notation");
