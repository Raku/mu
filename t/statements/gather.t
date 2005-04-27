#!/usr/bin/pugs

use v6;
use Test;

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
	is(+@a, 5, "5 elements gathered", :todo);
	ok($i, "gather code executed", :todo);
	is(@a[0], 1, "first elem taken", :todo);
	fail "uncatchable die", :todo;
	# XXX Because of release preparation
	# is(@a[-1], 5, "last elem taken", :todo);
};
