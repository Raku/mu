#!/usr/bin/pugs

use v6;
require Test;

plan 4;

my %foo;
my %e = ("foo", "bar", "blah", "blah");

todo_eval_ok('
	%foo = (
		"foo", "bar",
		"blah", "blah",
	);
	1;
', "expression parsed (well, lexed)");

todo_is(+%foo, +%e, "oh boy, it evaluates correctly, too");
todo_is(%foo<foo>, %e<foo>, "...");
todo_is(%foo<blah>, %e<blah>, "...");

