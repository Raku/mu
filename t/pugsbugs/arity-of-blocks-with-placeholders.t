#!/usr/bin/pugs

use v6;
use Test;

plan 9;

{
    is { $^a         }.arity, 1, "block with one placeholder var has .arity == 1";
    is { $^a,$^b     }.arity, 2, "block with one placeholder var has .arity == 2";
    is { $^a,$^b,$^c }.arity, 3, "block with one placeholder var has .arity == 3";
}

{
    is { my $k; $^a         }.arity, 1,
        "additional my() vars don't influence .arity calculation (1-1)";
    is { my $k; $^a,$^b     }.arity, 2,
        "additional my() vars don't influence .arity calculation (1-2)";
    is { my $k; $^a,$^b,$^c }.arity, 3,
        "additional my() vars don't influence .arity calculation (1-3)";
}

{
    is { $^a;         my $k }.arity, 1,
        "additional my() vars don't influence .arity calculation (2-1)";
    is { $^a,$^b;     my $k }.arity, 2,
        "additional my() vars don't influence .arity calculation (2-2)";
    is { $^a,$^b,$^c; my $k }.arity, 3,
        "additional my() vars don't influence .arity calculation (2-3)";
}
