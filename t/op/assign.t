#!/usr/bin/pugs

use v6;
require Test;

plan 2;

{
	my ($foo, $bar) = ("FOO", "BAR");
	is($foo, "FOO", "assigned correct value to first of two scalars");
	is($bar, "BAR", "... and second");
};

