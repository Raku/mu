#!/usr/bin/pugs

use v6;
require Test;

plan 3;

# testing the @*INC magical

ok(+@*INC > 0, 'we have something in our @INC');

my $number_in_inc = +@*INC;
push @*INC, 'test';
is(+@*INC, $number_in_inc + 1, 'we added something to @INC');

pop @*INC;
is(+@*INC, $number_in_inc, 'we removed something from @INC');
