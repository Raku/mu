#!/usr/bin/pugs

use v6;
require Test;

plan 3;

{
	my ($a, $b, $e);

	eval '
		my $const is constant = 2;
		$a = $const;
		$e = eval q( $const = 3 );
		$b = $const;
	';

	todo_is($a, 2, "constant initially 2");
	ok(!$e, "modifying constant is not allowed");
	todo_is($b, 2, "constant is still 2");
}

