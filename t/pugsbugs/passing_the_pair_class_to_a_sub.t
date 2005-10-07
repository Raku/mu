#!/usr/bin/pugs

use v6;
use Test;

plan 2;

{
    my sub foo ($x) { defined $x }

    my $pair = (a => 1);
    my $Pair = $pair.ref;

    ok try { foo($Pair) }, "passing ::Pair to a sub works", :todo<bug>;
}

# But this works:
{
    my sub foo ($x) { defined $x }

    my $int = 42;
    my $Int = $int.ref;

    ok try { foo($Int) }, "passing ::Int to a sub works";
}
