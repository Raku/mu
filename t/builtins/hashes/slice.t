#!/usr/bin/pugs

use v6;
require Test;

=kwid 

Testing hash slices.

=cut

plan 4;

{   my %hash = (1=>2,3=>4,5=>6);
    my @s=(2,4,6);

    ok(~@s eq ~%hash{(1,3,5)},             "basic slice");
    ok(~@s eq ~%hash{%hash.keys}.sort,     "values emulation slice, try 1");
    ok(~@s eq ~%hash{%hash.keys.sort},     "values emulation slice, try 2");
    ok(~@s eq ~%hash{(1,2,3)>>+<<(0,1,2)}, "list calculation slice");
}
