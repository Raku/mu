#!/usr/bin/pugs

use v6;
require Test;

=pod

Tests for the pipe operators 

    ==> and <== 
    
L<S06/"Pipe operators">
L<S03/"Piping operators">    

=cut

plan 7;

{
	my @a = (1, 2);
	my (@b, @c);
	
	eval '@a ==> @b';
	#eval '@c <== @a'; # this yields the error:
	#Fail: cannot cast into a handle: VList [VInt 1,VInt 2]

	is(~@b, ~@a, "ltr pipe as simple assignment", :todo(1));
	is(~@c, ~@a, "rtl pipe as simple assignment", :todo(1));
};

{
	my @a = (1 .. 5);
	my @e = (2, 4);

	my (@b, @c);
	eval '@a ==> grep { ($_ % 2) == 0 } ==> @b';
	#eval '@c <== grep { ($_ % 2) == 0 } <== @a';

	is(~@b, ~@e, "array ==> grep ==> result", :todo(1));
	is(~@c, ~@e, "result <== grep <== array", :todo(1));
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

	is($got_x, "x", "x was passed as explicit param", :todo(1));
	is($got_y, undef, "optional param y was not bound to piped list");
	is(~@got_z, ~@a, '...slurpy array *@z got it', :todo(1));
};
