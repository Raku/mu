#!/usr/bin/pugs

use v6;
require Test;

plan 10;

is(int('-1.999'), -1, "int('-1.999') is -1");
is(int('0x123'), 0x123, "int('0x123') is 0x123");
is(int('0d456'), 0d456, "int('0d456') is 0d456");
is(int('0o678'), 0o67, "int('0o678') is 0o67");
is(int('3e4d5'), 3e4, "int('3e4d5') is 3e4");

is(+'1.9e3', 1900, "+'1.9e3' is 1900");
is(+'Inf', Inf, "+'Inf' is Inf");
is(+'Info', 0, "+'Info' is 0");
is(+'NaN', NaN, "+'NaN' is NaN");
is(+'NaNa', 0, "+'NaNa' is 0");

