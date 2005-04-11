#!/usr/bin/pugs

use v6;
require Test;

plan 5;

{
	my @a;
	my $i;
	
	eval '@a = gather {
		$i = 1;
		for (1 .. 5) -> $j {
			take $j;
		}
	}';

	ok(!$i, "not yet gathered");
	todo_is(+@a, 5, "5 elements gathered");
	todo_ok($i, "gather code executed");
	todo_is(@a[0], 1, "first elem taken");
	todo_fail "uncatchable die";
	# XXX Because of release preparation
	# todo_is(@a[-1], 5, "last elem taken");
};
