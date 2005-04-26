#!/usr/bin/pugs

use v6;
use Test;

plan 4;

our %foo;
my %e = ("foo", "bar", "blah", "blah");

eval_ok('
	%foo = (
		"foo", "bar",
		"blah", "blah",
	);
	1;
', "expression parsed (well, lexed)");

is(+%foo, +%e, "oh boy, it evaluates correctly, too");
is(%foo<foo>, %e<foo>, "...");
is(%foo<blah>, %e<blah>, "...");

