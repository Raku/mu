#!/usr/bin/pugs

use v6;
use Test;

plan 6;

{
	my %hash; eval '%hash = enum «:Mon(1) Tue Wed Thu Fri Sat Sun»';

	#is((%hash<Mon Tue Wed Thu Fri Sat Sun>) »eq« (1 .. 7)), "enum generated correct sequence");
	is(%hash<Mon>, 1, "first value ok", :todo);
	is(%hash<Thu>, 4, "fourth value ok", :todo);
	is(%hash<Sun>, 7, "last value ok", :todo);
};

{
	my %hash; eval '%hash = enum «:Two(2) Three Four»';

	#is((%hash<Two Three Four>) »eq« (2 .. 4)), "enum generated correct sequence");
	is(%hash<Two>, 2, "first value ok", :todo);
	is(%hash<Three>, 3, "second value ok", :todo);
	is(%hash<Four>, 4, "last value ok", :todo);
};

