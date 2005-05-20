#!/usr/bin/pugs

use v6;
use Test;

plan 5;

{
	my @a;
	my $i;
	
        @a = gather {
		$i = 1;
		for (1 .. 5) -> $j {
			take $j;
		}
	};

	ok(!$i, "not yet gathered", :todo<unspecced>);
	is(+@a, 5, "5 elements gathered");
	ok($i, "gather code executed");
	is(@a[0], 1, "first elem taken");
	is(@a[-1], 5, "last elem taken");
};
