#!/usr/bin/pugs

use v6;
require Test;

plan 32;

is(int(-1), -1, "int(-1) is -1");
is(int(0), 0, "int(0) is 0");
is(int(1), 1, "int(1) is 1");
is(int(3.14159265), 3, "int(3.14159265) is 3");
is(int(-3.14159265), -3, "int(-3.14159265) is -3");

is(int(0.999), 0, "int(0.999) is 0");
is(int(0.51), 0, "int(0.51) is 0");
is(int(0.5), 0, "int(0.5) is 0");
is(int(0.49), 0, "int(0.49) is 0");
is(int(0.1), 0, "int(0.1) is 0");

is(int(-0.999), -0, "int(-0.999) is -0");
is(int(-0.51), -0, "int(-0.51) is -0");
is(int(-0.5), -0, "int(-0.5) is -0");
is(int(-0.49), -0, "int(-0.49) is -0");
is(int(-0.1), -0, "int(-0.1) is -0");

is(int(1.999), 1, "int(1.999) is 1");
is(int(1.51), 1, "int(1.51) is 1");
is(int(1.5), 1, "int(1.5) is 1");
is(int(1.49), 1, "int(1.49) is 1");
is(int(1.1), 1, "int(1.1) is 1");

is(int(-1.999), -1, "int(-1.999) is -1");
is(int(-1.51), -1, "int(-1.51) is -1");
is(int(-1.5), -1, "int(-1.5) is -1");
is(int(-1.49), -1, "int(-1.49) is -1");
is(int(-1.1), -1, "int(-1.1) is -1");

is(int('-1.999'), -1, "int('-1.999') is -1");
is(int('0x123'), 0x123, "int('0x123') is 0x123");
is(int('0d456'), 0d456, "int('0d456') is 0d456");
is(int('0o678'), 0o67, "int('0o678') is 0o67");
is(int('3e4d5'), 3e4, "int('3e4d5') is 3e4");

is(+'Inf', Inf, "+'Inf' is Inf");
is(+'Infamy', 0, "+'Infamy' is 0");

