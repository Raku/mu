#!/usr/bin/pugs

use v6;
require Test;

plan 11;

my @foo
ok @foo = eval 'split "", "forty-two"', "split evaluated";
is +@foo, 9, "split created the correct number of elements";
is @foo[0], "f";
is @foo[1], "o";
is @foo[2], "r";
is @foo[3], "t";
is @foo[4], "y";
is @foo[5], "-";
is @foo[6], "t";
is @foo[7], "w";
is @foo[8], "o";
