#!/usr/bin/pugs

use v6;
use Test;

plan 3;

{
	my ($a, $b, $e);

	eval '
		my $const is constant = 2;
		$a = $const;
		$e = eval q( $const = 3 );
		$b = $const;
	';

	is($a, 2, "constant initially 2");
	ok(!$e, "modifying constant is not allowed");
	is($b, 2, "constant is still 2", :todo);
}

