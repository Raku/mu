#!/usr/bin/pugs

use v6;
require Test;

plan 11;

my @foo;
ok @foo = eval 'split "", "forty-two"', "split evaluated";
is +@foo, 9, "split created the correct number of elements";
is @foo[0], "f", 'the first value of the split array is ok';
is @foo[1], "o", 'the second value of the split array is ok';
is @foo[2], "r", 'the third value of the split array is ok';
is @foo[3], "t", 'the fourth value of the split array is ok';
is @foo[4], "y", 'the fifth value of the split array is ok';
is @foo[5], "-", 'the sixth value of the split array is ok';
is @foo[6], "t", 'the seventh value of the split array is ok';
is @foo[7], "w", 'the eighth value of the split array is ok';
is @foo[8], "o", 'the ninth value of the split array is ok';
