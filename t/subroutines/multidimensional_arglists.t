#!/usr/bin/pugs

use v6;
use Test;

plan 7;

# L<S06/"Multidimensional argument list binding">

sub get_multidim_arglist (*@;AoA) { @;AoA }

{
    my @array1 = <a b c>;
    my @array2 = <d e f>;

    my @AoA = try { get_multidim_arglist(@array1, @array2) };
    is +@AoA,          2, "basic multidim arglist binding (1)", :todo<feature>;
    is ~@AoA[0], "a b c", "basic multidim arglist binding (2)", :todo<feature>;
    is ~@AoA[1], "d e f", "basic multidim arglist binding (3)", :todo<feature>;
}

{
    my @array1 = <a b c>;

    my @AoA = try { get_multidim_arglist(@array1) };
    is +@AoA,          1, "multidim arglist binding with only one array (1)", :todo<feature>;
    is ~@AoA[0], "a b c", "multidim arglist binding with only one array (2)", :todo<feature>;
}

{
    dies_ok { get_multidim_arglist(1,2,3) },
        "three scalars are not compatible with a sub expecting a multidim arglist", :todo<feature>;
}

# Unspecced
{
    my $array1 = <a b c>;
    my $array2 = <d e f>;

    dies_ok { get_multidim_arglist($array1, $array2) },
        "two arrayrefs are not compatible with a sub expecting a multidim arglist", :todo<feature>;
}
