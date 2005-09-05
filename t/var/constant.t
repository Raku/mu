#!/usr/bin/pugs

use v6;
use Test;

plan 5;

# Following tests test whether the declaration succeeded.
{
    my $ok;

    eval '
        constant foo = 42;
        $ok = foo == 42;
    ';

    ok $ok, "declaring a sigilless constant using 'constant' works";
}

{
    my $ok;

    eval '
        constant $bar = 42;
        $ok = $bar == 42;
    ';

    ok $ok, "declaring a constant with a sigil using 'constant' works";
}

{
    my $ok;

    eval '
        constant Num baz = 42;
        $ok = baz == 42;
    ';

    ok $ok, "declaring a sigilless constant with a type specification using 'constant' works";
}

{
    my $ok;

    eval '
        constant λ = 42;
        $ok = λ == 42;
    ';

    ok $ok, "declaring an Unicode constant using 'constant' works";
}

# Following tests test whether the constants are actually constant.
{
    my $ok;

    eval '
        constant grtz = 42;
        $ok++ if grtz == 42;

        try { grtz = 23 };
        $ok++ if $!;
    ';

    is $ok, 2, "a constant declared using 'constant' is actually constant";
}

=for discussion

# See thread "our constant pi, my constant pi" on p6l started by Ingo
# Blechschmidt: http://www.nntp.perl.org/group/perl.perl6.language/23000

{
    my $ok;

    eval '
        {
            my constant grtz = 42;
            $ok++ if grtz == 42;
        }

        $ok++ unless eval 'grtz; 1';
    ';

    is $ok, 2, "declaring constants using 'my constant' works";
}

{
    my $ok;

    eval '
        my constant grtz = 42;
        $ok++ if grtz == 42;

        {
            my constant grtz = 23;
            $ok++ if grtz == 23;
        }

        $ok++ if grtz == 23;
    ';

    is $ok, 3, "constants declared by 'my constant' shadow correctly";
}

{
    my $ok;

    eval '
        {
            our constant grtz = 42;
            $ok++ if grtz == 42;
        }

        $ok++ if grtz;
    ';

    is $ok, 2, "declaring constants using 'our constant' works";
}
