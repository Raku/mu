#!/usr/bin/pugs

use v6;
require Test;

plan 6;

{
	my %hash; eval '%hash = enum «:Mon(1) Tue Wed Thu Fri Sat Sun»';

	#is((%hash<Mon Tue Wed Thu Fri Sat Sun>) »eq« (1 .. 7)), "enum generated correct sequence");
	todo_is(%hash<Mon>, 1, "first value ok");
	todo_is(%hash<Thu>, 4, "fourth value ok");
	todo_is(%hash<Sun>, 7, "last value ok");
};

{
	my %hash; eval '%hash = enum «:Two(2) Three Four»';

	#is((%hash<Two Three Four>) »eq« (2 .. 4)), "enum generated correct sequence");
	todo_is(%hash<Two>, 2, "first value ok");
	todo_is(%hash<Three>, 3, "second value ok");
	todo_is(%hash<Four>, 3, "last value ok");
};

