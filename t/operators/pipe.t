#!/usr/bin/pugs

use v6;
require Test;


plan 7;

{
	my @a = (1, 2);
	my (@b, @c);
	
	eval '@a ==> @b';
	#eval '@c <== @a'; # this yields the error:
	#Fail: cannot cast into a handle: VList [VInt 1,VInt 2]

	todo_is(~@b, ~@a, "ltr pipe as simple assignment");
	todo_is(~@c, ~@a, "rtl pipe as simple assignment");
};

{
	my @a = (1 .. 5);
	my @e = (2, 4);

	my (@b, @c);
	eval '@a ==> grep { ($_ % 2) == 0 } ==> @b';
	#eval '@c <== grep { ($_ % 2) == 0 } <== @a';

	todo_is(~@b, ~@e, "array ==> grep ==> result");
	todo_is(~@c, ~@e, "result <== grep <== array");
};

{
	my ($got_x, $got_y, @got_z);
	sub foo ($x, ?$y, *@z) {
		$got_x = $x;
		$got_y = $y;
		@got_z = @z;
	}

	my @a = (1 .. 5);

	eval '@a ==> foo "x"';

	todo_is($got_x, "x", "x was passed as explicit param");
	is($got_y, undef, "optional param y was not bound to piped list");
	todo_is(~@got_z, ~@a, '...slurpy array *@z got it');
};
