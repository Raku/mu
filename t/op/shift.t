#!/usr/bin/pugs

use v6;
require Test;

plan(6);

my @s = (1, 2, 3, 4, 5);

is(shift(@s), 1, "shift");
todo_is(eval 'shift(@s)', 2, "shift");
todo_is(eval 'shift(@s)', 3, "shift");
todo_is(eval 'shift(@s)', 4, "shift");
todo_is(eval '@s.shift', 5, "shift method");
ok(!defined(shift(@s)), "shift");
