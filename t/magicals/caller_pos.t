#!/usr/bin/pugs

use v6;
use Test;

plan 7;

is($?POSITION, "$?FILE at line 8, column 1", 'plain old $?POSITION');

sub foo { return "$?CALLER::POSITION" }
sub bar { gorch("$?CALLER::POSITION") }
sub gorch (Str $str) { return $str }

is(foo(), "$?FILE at line 14, column 1", "basic caller position interpolation");

# in the second test case the position is evaluated where gorch is concerned
# that is, bar's call to gorch().

# the variable is interpolated in bar(), and that's where in the call stack
# this should be relevant to.
is(bar(), "$?FILE at line 21, column 1", "indirect interpolation (wtf?!)");

sub inner { return join("\n", $?CALLER::CALLER::CALLER::POSITION, $?CALLER::CALLER::POSITION, $?CALLER::POSITION) }
sub outer { inner() }
sub very  { outer() }

is(very(), "$?FILE at line 27, column 1\n$?FILE at line 25, column 13\n$?FILE at line 24, column 13", "caller::caller notation works");

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
is(very2(), "$?FILE at line 45, column 1\n$?FILE at line 41, column 11\n$?FILE at line 37, column 12", "caller::caller notation works across multiple lines");

sub line_set { return "$?CALLER::POSITION" }
#line 666
sub get_line { return line_set };
is(get_line, "$?FILE at line 667, column 16\n", "#line works for caller::caller notation", :todo(1));
