#!/usr/bin/pugs

use v6;
require Test;

plan(6);

my @s = (1, 2, 3, 4, 5);

todo_ok (eval 'shift(@s)' == 1, "shift");
todo_ok (eval 'shift(@s)' == 2, "shift");
todo_ok (eval 'shift(@s)' == 3, "shift");
todo_ok (eval 'shift(@s)' == 4, "shift");
todo_ok (eval '@s.shift'  == 5, "shift method");
todo_ok (eval '!defined(shift(@s))', "shift");
