#!/usr/bin/pugs

use v6;
require Test;

plan 9;

{
	my @a = (0, 2, 4);
	my @b = (1, 3, 5);

	my @e = (0 .. 5);

	my @z; eval '@z = zip(@a; @b)';
	my @y; eval '@y = @a ¥ @b';
	my @x; eval '@x = @a Y @b';

	todo_is(~@z, ~@e, "simple zip");
	todo_is(~@y, ~@e, "also with yen char");
	todo_is(~@x, ~@e, "also with Y char");
};

{
	my @a = (0, 3);
	my @b = (1, 4);
	my @c = (2, 5);

	my @e = (0 .. 5);

	my @z; eval '@z = zip(@a; @b; @c)';
	my @y; eval '@y = @a ¥ @b ¥ @c';
	my @x; eval '@x = @a Y @b Y @c';

	todo_is(~@z, ~@e, "zip of 3 arrays");
	todo_is(~@y, ~@e, "also with yen char");
	todo_is(~@x, ~@e, "also with yen char");
};

{
	my @a = (0, 4);
	my @b = (2, 6);	
	my @c = (1, 3, 5, 7);

	my @e = (0 .. 7);

	my @z; eval '@z = zip(zip(@a; @b); @c)';
	my @y; eval '@y = (@a ¥ @b) ¥ @c';
	my @x; eval '@x = (@a Y @b) Y @c';

	todo_is(~@z, ~@e, "zip of zipped arrays with other array");
	todo_is(~@y, ~@e, "also as ¥");
	todo_is(~@x, ~@e, "also as Y");
};
