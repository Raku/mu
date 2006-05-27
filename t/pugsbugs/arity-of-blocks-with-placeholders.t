#!/usr/bin/pugs

use v6;
use Test;

plan 9;

{
    is ({ $^a         }.arity), 1, "block with one placeholder var has .arity == 1";
    is arity({ $^a,$^b     }), 2, "block with one placeholder var has .arity == 2";
    is arity({ $^a,$^b,$^c }), 3, "block with one placeholder var has .arity == 3";
}

{
    is arity({ my $k; $^a         }), 1,
        "additional my() vars don't influence .arity calculation (1-1)";
    is arity({ my $k; $^a,$^b     }), 2,
        "additional my() vars don't influence .arity calculation (1-2)";
    is arity({ my $k; $^a,$^b,$^c }), 3,
        "additional my() vars don't influence .arity calculation (1-3)";
}

{
    is arity({ $^a;         my $k }), 1,
        "additional my() vars don't influence .arity calculation (2-1)";
    is arity({ $^a,$^b;     my $k }), 2,
        "additional my() vars don't influence .arity calculation (2-2)";
    is arity({ $^a,$^b,$^c; my $k }), 3,
        "additional my() vars don't influence .arity calculation (2-3)";
}
