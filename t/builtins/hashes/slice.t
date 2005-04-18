#!/usr/bin/pugs

use v6;
require Test;

=kwid 

Testing hash slices.

=cut

plan 4;

{   my %hash = (1=>2,3=>4,5=>6);
    my @s=(2,4,6);

    ok(~@s eq ~%hash{1,3,5},               "basic slice");
    ok(~@s eq ~%hash{(1,3,5)},             "basic slice, explicit list");
    ok(~@s eq ~%hash<1 3 5>,               "basic slice, <> syntax");

    ok(~%hash{1,1,5,1,3} eq "2 2 6 2 4",   "basic slice, duplicate keys");
    ok(~%hash<1 1 5 1 3> eq "2 2 6 2 4",   "basic slice, duplicate keys, <> syntax");

    ok(~@s eq ~%hash{%hash.keys}.sort,     "values from hash keys, part 1");
    ok(~@s eq ~%hash{%hash.keys.sort},     "values from hash keys, part 2");
    ok(~@s eq ~%hash{(1,2,3)>>+<<(0,1,2)}, "calculated slice: hyperop");

}
