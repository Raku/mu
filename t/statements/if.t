#!/usr/bin/pugs

use v6;
use Test;

=kwid

Basic "if" tests.

L<S04/"Conditional statements">

=cut

plan 16;
force_todo 8;

my $x = 'test';
if ($x eq $x) { pass("if ($x eq $x) {} works"); } else { fail("if ($x eq $x) {} failed"); }
if ($x ne $x) { fail("if ($x ne $x) {} failed"); } else { pass("if ($x ne $x) {} works"); }
if (1) { pass("if (1) {} works"); } else { fail("if (1) {} failed"); }
if (0) { fail("if (0) {} failed"); } else { pass("if (0) {} works"); }
if (undef) { fail("if (undef) {} failed"); } else { pass("if (undef) {} works"); }

# die called in the condition part of an if statement should die immediately
# rather than being evaluated as true
my $foo = 1;
eval 'if (die "should die") { $foo = 3 } else { $foo = 2; }';
#say '# $foo = ' ~ $foo;
is $foo, 1, "die should stop execution immediately.";

{
	my $foo = 1; # just in case
	eval 'if 1 > 2 { $foo = 2 } else { $foo = 3 }';
	is $foo, 3, 'if with no parens';
};

{
	my $foo = 1;
	eval 'if { 1 > 0 } { $foo = 2 } else { $foo = 3 }';
	is $foo, 2, 'if with no parens, and closure as cond';
	### This is a parser problem.  This test has been copied to perlbugs.
};

# if...elsif
{
	my $foo = 1;
	eval 'if (1) { $foo = 2 } elsif (1) { $foo = 3 }';
	is $foo, 2, 'if (1) {} elsif (1) {}';
}

{
	my $foo = 1;
	eval 'if (1) { $foo = 2 } elsif (0) { $foo = 3 }';
	is $foo, 2, 'if (1) {} elsif (0) {}';
}

{
	my $foo = 1;
	eval 'if (0) { $foo = 2 } elsif (1) { $foo = 3 }';
	is $foo, 3, 'if (0) {} elsif (1) {}';
}

{
	my $foo = 1;
	eval 'if (0) { $foo = 2 } elsif (0) { $foo = 3 }';
	is $foo, 1, 'if (0) {} elsif (0) {}';
}

# if...elsif...else

{
	my $foo = 1;
	my $c = 'if (0) { $foo = 2 } elsif (0) { $foo = 3 } else { $foo = 4 }';
	eval $c;
	is $foo, 4, $c;
}

{
	my $foo = 1;
	my $c = 'if (1) { $foo = 2 } elsif (0) { $foo = 3 } else { $foo = 4 }';
	eval $c;
	is $foo, 2, $c;
}

{
	my $foo = 1;
	my $c = 'if (1) { $foo = 2 } elsif (1) { $foo = 3 } else { $foo = 4 }';
	eval $c;
	is $foo, 2, $c;
}

{
	my $foo = 1;
	my $c = 'if (0) { $foo = 2 } elsif (1) { $foo = 3 } else { $foo = 4 }';
	eval $c;
	is $foo, 3, $c;
}

