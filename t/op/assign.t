#!/usr/bin/pugs

use v6;
require Test;

plan 9;

{
	my ($foo, $bar) = ("FOO", "BAR");
	is($foo, "FOO", "assigned correct value to first of two scalars");
	is($bar, "BAR", "... and second");
};

{
	my @a;
	@a[1, 2, 3] = (100, 200, 300);
	is(@a[1], 100, "assigned correct value from list to sliced list");
	is(@a[2], 200, "... and second");
	is(@a[3], 300, "... and third");
	is(@a[0], undef, "won't modify unassigned one");

	my @b;
	@b[2, 1, 0] = (401, 201, 1);
	is(@b[0], 1, "assigned correct value from list to unsorted sliced list");
	is(@b[1], 201, "... and second");
	is(@b[2], 401, "... and third");
}

