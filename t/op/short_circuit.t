#!/usr/bin/pugs

use v6;
require Test;

=pod

Tests that || and && really short circuit, and do not call their rhs when the
lhs is enough to deduce the result.

=cut

# test cases by Andrew Savige

plan 2;

{
	my $x = 1;
	my $y = 2;
	$x == 1 or $y = 42;

	todo_is($y, 2, "assignment was not changed in unreached short circuiting expr");
}


{
	my $x = 1;
	my $y = 2;
	$x != 1 && ($y = 42);

	todo_is($y, 2, "assignment was not changed in unreached short circuiting expr");
}

