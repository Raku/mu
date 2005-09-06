#!/usr/bin/pugs

use v6;
use Test;

plan 12;

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
        $ok++ if grtz == 42;
    ';

    is $ok, 3, "a constant declared using 'constant' is actually constant (1)";
}

{
    my $ok;

    eval '
        constant baka = 42;
        $ok++ if baka == 42;

        try { baka := 23 };
        $ok++ if $!;
        $ok++ if baka == 42;
    ';

    is $ok, 3, "a constant declared using 'constant' is actually constant (2)";
}

{
    my $ok;

    eval '
        constant wobble = 42;
        $ok++ if wobble == 42;

        try { &wobble = { 23 } };
        $ok++ if $!;
        $ok++ if wobble == 42;
    ';

    is $ok, 3, "a constant declared using 'constant' is actually constant (3)";
}

{
    my $ok;

    eval '
        constant wibble = 42;
        $ok++ if wibble == 42;

        try { &wibble := { 23 } };
        $ok++ if $!;
        $ok++ if wibble == 42;
    ';

    is $ok, 3, "a constant declared using 'constant' is actually constant (4)";
}

# See thread "our constant pi, my constant pi" on p6l started by Ingo
# Blechschmidt (http://www.nntp.perl.org/group/perl.perl6.language/23000),
# especially Luke's reply
# (http://www.nntp.perl.org/group/perl.perl6.language/23000).

{
    my $ok;

    eval '
        {
            my constant wack = 42;
            $ok++ if wack == 42;
        }

        $ok++ unless eval "wack; 1";
    ';

    is $ok, 2, "declaring constants using 'my constant' works";
}

{
    my $ok;

    eval '
        my constant wack = 42;
        $ok++ if wack == 42;

        {
            my constant wack = 23;
            $ok++ if wack == 23;
        }

        $ok++ if wack == 23;
    ';

    is $ok, 3, "constants declared by 'my constant' shadow correctly";
}

{
    my $ok;

    eval '
        {
            our constant globconst1 = 42;
            $ok++ if globconst1 == 42;
        }

        $ok++ if globconst1 == 42;
    ';

    is $ok, 2, "declaring constants using 'our constant' works";
}

{
    my $ok;

    eval '
        {
            constant globconst2 = 42;
            $ok++ if globconst2 == 42;
        }

        $ok++ if globconst2 == 42;
    ';

    is $ok, 2, "declaring constants using 'constant' creates package-scoped vars";
}
