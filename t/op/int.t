#!/usr/bin/pugs

use v6;
require Test;

plan 25;

is(int(-1), -1, "int(-1) is -1");
is(int(0), 0, "int(0) is 0");
is(int(1), 1, "int(1) is 1");
is(int(3.14159265), 3, "int(3.14159265) is 3");
is(int(-3.14159265), -3, "int(-3.14159265) is -3");

todo_is(int(0.999), 0, "int(0.999) is 0");
todo_is(int(0.51), 0, "int(0.51) is 0");
is(int(0.5), 0, "int(0.5) is 0");
is(int(0.49), 0, "int(0.49) is 0");
is(int(0.1), 0, "int(0.1) is 0");

todo_is(int(-0.999), -0, "int(-0.999) is -0");
todo_is(int(-0.51), -0, "int(-0.51) is -0");
is(int(-0.5), -0, "int(-0.5) is -0");
is(int(-0.49), -0, "int(-0.49) is -0");
is(int(-0.1), -0, "int(-0.1) is -0");

todo_is(int(1.999), 1, "int(1.999) is 1");
todo_is(int(1.51), 1, "int(1.51) is 1");
todo_is(int(1.5), 1, "int(1.5) is 1");
is(int(1.49), 1, "int(1.49) is 1");
is(int(1.1), 1, "int(1.1) is 1");

todo_is(int(-1.999), -1, "int(-1.999) is -1");
todo_is(int(-1.51), -1, "int(-1.51) is -1");
todo_is(int(-1.5), -1, "int(-1.5) is -1");
is(int(-1.49), -1, "int(-1.49) is -1");
is(int(-1.1), -1, "int(-1.1) is -1");

