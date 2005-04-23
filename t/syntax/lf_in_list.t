#!/usr/bin/pugs

use v6;
require Test;

plan 4;

my %foo;
my %e = ("foo", "bar", "blah", "blah");

eval_ok('
	%foo = (
		"foo", "bar",
		"blah", "blah",
	);
	1;
', "expression parsed (well, lexed)", :todo(1));

is(+%foo, +%e, "oh boy, it evaluates correctly, too", :todo(1));
is(%foo<foo>, %e<foo>, "...", :todo(1));
is(%foo<blah>, %e<blah>, "...", :todo(1));

