#!/usr/bin/pugs

use v6;
require Test;

=pod

Tests that || and && really short circuit, and do not call their rhs when the
lhs is enough to deduce the result.

=cut

# test cases by Andrew Savige

plan 12;

{
    my $x = 1;
    my $y = 2;
    $x == 1 || ($y = 42);

    is($y, 2, "|| operator seems to be short circuiting");
}

{
    my $x = 1;
    my $y = 2;
    $x == 1 or $y = 42;

    is($y, 2, "'or' operator seems to be short circuiting");
}

{
    my $x = 1;
    my $y = 2;
    $x != 1 && ($y = 42);

    is($y, 2, "&& operator seems to be short circuiting");
}

{
    my $x = 1;
    my $y = 2;
    $x != 1 and $y = 42;

    is($y, 2, "'and' operator seems to be short circuiting");
}

{
    my $x = 1;
    my $y = 2;
    $x // ($y = 42);

    is($y, 2, "// operator seems to be short circuiting");
}

{
    my $x = 1;
    my $y = 2;
    $x err $y = 42;

    is($y, 2, "'err' operator seems to be short circuiting");
}

{
    my $x;      # should be undef
    my $y = 2;
    $x // ($y = 42);

    is($y, 42, "// operator seems to be working");
}

{
    my $x;      # should be undef
    my $y = 2;
    $x err $y = 42;

    is($y, 42, "'err' operator seems to be working");
}

{
    my $x0 = 0;
    my @a0 = () and $x0 = 1;
    is($x0, 0,    "'and' operator seems to be short circuiting");
    ok(+@a0 == 0, "'and' operator seems to be working with list assignment");
    # cmp_ok(+@a0, '==', 0, "'or' operator seems to be working with list assignment");
}

{
    my $x0 = 0;
    my @a0 = () or $x0 = 1;
    is($x0, 1,    "'or' operator seems to be short circuiting");
    ok(+@a0 == 0, "'or' operator seems to be working with list assignment"); # unTODOme
    # cmp_ok(+@a0, '==', 0, "'or' operator seems to be working with list assignment");
}
