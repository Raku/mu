#!/usr/bin/pugs

use v6;
require Test;

plan 6;

{
	my @a = (4, 5, 3, 2, 5, 1);
	my @e = (1 .. 5, 5);

	my @s; eval '@s = @a.sort';
	is(@s, @e, 'array of numbers was sorted');
};

{
	my @a = (2, 45, 6, 1, 3);
	my @e = (1, 2, 3, 6, 45);

	my @s; eval '@s = @a.sort:{ $^a <=> $^b }';
	todo_is(@s, @e, '... with explicit spaceship');
};

{
	my @a = <foo bar gorch baz>;
	my @e = <bar baz foo gorch>;

	my @s; eval '@s = @a.sort';
	is(@s, @e, 'array of strings was sorted');
};

{
	my @a = <daa boo gaa aaa>;
	my @e = <aaa boo daa gaa>;

	my @s; eval '@s = @a.sort:{ $^a cmp $^b }';
	todo_is(@s, @e, '... with explicit cmp');
};

{
	my @a = <foo bar>;
	my @e = <bar foo>;

	my @s; eval '@s = sort @a';
	is(@s, @e, 'with non dotty syntax');
};

{
	my @a = (4, 1, 2, 5, 3);
	my @e = (1 .. 5);

	my @s; eval '@s = sort { $^a <=> $^b }, @a';
	todo_is(@s, @e, '... with explicit sort block');
};
