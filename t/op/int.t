#!/usr/bin/pugs

use v6;
require Test;

plan(5);

my $x = 3.14159265;
my $y = -3.14159265;

is(int(-1), -1);
is(int(0), 0);
is(int(1), 1);
is(int($x), 3);
is(int($y), -3);
