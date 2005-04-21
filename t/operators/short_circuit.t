#!/usr/bin/pugs

use v6;
require Test;

=pod

Tests that || and && and // really short circuit, and do not call their
rhs when the lhs is enough to deduce the result.

Also, test new ^^ operator here: even though it does not short circuit,
it is closely related to || and && and //.

=cut

# test cases by Andrew Savige

plan 30;
force_todo 30;

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
    my $x;      # should be undef
    my $y = 2;
    $x ^^ ($y = 42);

    is($y, 42, "^^ operator seems to be not short circuiting");
}

{
    my $x;      # should be undef
    my $y = 2;
    $x xor $y = 42;

    is($y, 42, "xor operator seems to be not short circuiting");
}

{
    is(1 && 42,      42, "&&   operator seems to be working");
    is((1 and 42),     42, "and  operator seems to be working");

    is(0 || 42,      42, "||   operator seems to be working");
    is((0 or 42),      42, "or   operator seems to be working");

    is((undef() // 42),  42, "//   operator seems to be working"); #"
    is((undef() err 42), 42, "err  operator seems to be working");

    is((0 ^^ 42),  42, "^^  operator seems to be working (one true)");
    is((42 ^^ 0),  42, "^^  operator seems to be working (one true)");
    ok(!(1 ^^ 42),   "^^  operator seems to be working (both true)");
    ok(!(0 ^^ 0),    "^^  operator seems to be working (both false)");
    is((0 xor 42), 42, "xor operator seems to be working (one true)");
    is((42 xor 0), 42, "xor operator seems to be working (one true)");
    is((0 xor 42), 42, "xor operator seems to be working (one true)");
    is((42 xor 0), 42, "xor operator seems to be working (one true)");
    ok(!(1 xor 42),  "xor operator seems to be working (both true)");
    ok(!(0 xor 0),   "xor operator seems to be working (both false)");
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
    is(+@a0, 0, "'or' operator seems to be working with list assignment");
}
