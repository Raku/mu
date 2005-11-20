#!/usr/bin/pugs

use v6;
use Test;

plan 19;

# L<S03/"Operator renaming" /flipflop operator is now done with/>

# Following test copied from Perl 5.9.2's t/op/flip.t
{
    my @a = (1,2,3,4,5,6,7,8,9,10,11,12);
    my ($z, $y);

    for @a {
        my $x = ($_ == 4) till ($_ == 8);
        if $x { $z = $x }
        $y ~= (index($_, 1) != -1) till (index($_, 2) != -1);
    }

    is $z, '5E0';
    is $y, '12E0123E0';
}

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

# XXX what about appending "E0"? This is probably done with "... but
# end_of_sequence" now.
