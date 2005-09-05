#!/usr/bin/pugs

use v6;
use Test;

=kwid

Basic "if" tests.

L<S04/"Conditional statements">

=cut

plan 17;

my $x = 'test';
if ($x eq $x) { pass("if ($x eq $x) {} works"); } else { fail("if ($x eq $x) {} failed"); }
if ($x ne $x) { fail("if ($x ne $x) {} failed"); } else { pass("if ($x ne $x) {} works"); }
if (1) { pass("if (1) {} works"); } else { fail("if (1) {} failed"); }
if (0) { fail("if (0) {} failed"); } else { pass("if (0) {} works"); }
if (undef) { fail("if (undef) {} failed"); } else { pass("if (undef) {} works"); }

# die called in the condition part of an if statement should die immediately
# rather than being evaluated as true
my $foo = 1;
try { if (die "should die") { $foo = 3 } else { $foo = 2; } };
#say '# $foo = ' ~ $foo;
is $foo, 1, "die should stop execution immediately.";

{
    my $foo = 1; # just in case
    if 1 > 2 { $foo = 2 } else { $foo = 3 };
    is $foo, 3, 'if with no parens';
};

# if...elsif
{
    my $foo = 1;
    if (1) { $foo = 2 } elsif (1) { $foo = 3 };
    is $foo, 2, 'if (1) {} elsif (1) {}';
}

{
    my $foo = 1;
    if (1) { $foo = 2 } elsif (0) { $foo = 3 };
    is $foo, 2, 'if (1) {} elsif (0) {}';
}

{
    my $foo = 1;
    if (0) { $foo = 2 } elsif (1) { $foo = 3 };
    is $foo, 3, 'if (0) {} elsif (1) {}';
}

{
    my $foo = 1;
    if (0) { $foo = 2 } elsif (0) { $foo = 3 };
    is $foo, 1, 'if (0) {} elsif (0) {}';
}

# if...elsif...else

{
    my $foo = 1;
    if (0) { $foo = 2 } elsif (0) { $foo = 3 } else { $foo = 4 };
    is $foo, 4;
}

{
    my $foo = 1;
    if (1) { $foo = 2 } elsif (0) { $foo = 3 } else { $foo = 4 };
    is $foo, 2;
}

{
    my $foo = 1;
    if (1) { $foo = 2 } elsif (1) { $foo = 3 } else { $foo = 4 };
    is $foo, 2;
}

{
    my $foo = 1;
    if (0) { $foo = 2 } elsif (1) { $foo = 3 } else { $foo = 4 };
    is $foo, 3;
}

{
    my $foo = 1;
    eval 'if { 1 > 0 } { $foo = 2 } else { $foo = 3 }';
    is $foo, 2, 'if with no parens, and closure as cond',:todo<bug>;
    ### This is a parser problem.  This test has been copied to perlbugs.
};

# I'm not sure where this should go

{
    eval_is(
        'if( ( my $x = 2 ) == 2 ) { $x; }',
        2,
        "'my' variable within 'if' conditional",
    :todo<feature>);
}
