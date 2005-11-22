#!/usr/bin/pugs

use v6;
use Test;

plan 20;

# L<S03/"Operator renaming" /flipflop operator is now done with/>

sub take (Int $n, Code &f) { (1..$n).map:{ f() } }
sub always_false { 0 }
sub always_true  { 1 }

# Basic till
{
    my @result = take 5, { ?(always_false() till always_false()) };
    is ~@result, "    ", "always_false() till always_false()";
}

{
    my @result = take 5, { ?(always_false() till always_true()) };
    is ~@result, "    ", "always_false() till always_true()";
}

{
    my @result = take 5, { ?(always_true() till always_true()) };
    ok all(@result), "always_true() till always_true()";
}

{
    my @result = take 5, { ?(always_true() till always_false()) };
    is ~@result, "1 2 3 4 5", "always_true() till always_false()";
}

# Basic ^till
{
    my @result = take 5, { ?(always_false() ^till always_false()) };
    is ~@result, "    ", "always_false() ^till always_false()";
}

{
    my @result = take 5, { ?(always_false() ^till always_true()) };
    is ~@result, "    ", "always_false() ^till always_true()";
}

{
    my @result = take 5, { ?(always_true() ^till always_true()) };
    my $first  = shift @result;

    ok !$first && all(@result), "always_true() ^till always_true()";
}

{
    my @result = take 5, { ?(always_true() ^till always_false()) };
    is ~@result, " 2 3 4 5", "always_true() ^till always_false()";
}

# Basic till^
{
    my @result = take 5, { ?(always_false() till^ always_false()) };
    is ~@result, "    ", "always_false() till^ always_false()";
}

{
    my @result = take 5, { ?(always_false() till^ always_true()) };
    is ~@result, "    ", "always_false() till^ always_true()";
}

{
    my @result = take 5, { ?(always_true() till^ always_true()) };

    # XXX what should the result be?
}

{
    my @result = take 5, { ?(always_true() till^ always_false()) };
    is ~@result, "1 2 3 4 5", "always_true() till^ always_false()";
}

# RHS not evaluated when in "false" state (perldoc perlop, /flip-flop)
{
    lives_ok { always_false()  till  die() }, "RHS not evaluated in \"false\" state (till)";
    lives_ok { always_false() ^till  die() }, "RHS not evaluated in \"false\" state (^till)";
    lives_ok { always_false()  till^ die() }, "RHS not evaluated in \"false\" state (till^)";
}

# LHS not evaluated when in "true" state (perldoc perlop, /flip-flop)
{
    my sub true_then_die {
        state $invoked;
        unless $invoked++ {
            "true";
        } else {
            die;
        }
    }

    lives_ok { true_then_die()  till  always_false() },
        "LHS not evaluated in \"false\" state (till)";
    lives_ok { true_then_die() ^till  always_false() },
        "LHS not evaluated in \"false\" state (^till)";
    lives_ok { true_then_die()  till^ always_false() },
        "LHS not evaluated in \"false\" state (till^)";
}

# See thread "till (the flipflop operator, formerly ..)" on p6l started by Ingo
# Blechschmidt, especially Larry's reply:
# http://www.nntp.perl.org/group/perl.perl6.language/24098
{
    my sub foo ($x) { $x till 0 }

    ok !foo(0), "all sub invocations share the same till-state (1)";
    ok  foo(1), "all sub invocations share the same till-state (2)";
    ok  foo(0), "all sub invocations share the same till-state (3)";
}
