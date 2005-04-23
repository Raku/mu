#!/usr/bin/pugs

use v6;
require Test;

=kwid 

Nested array tests; various interactions of arrayrefs, arrays, flattening and nesting.

=cut

plan 8;

{   
    my @a = (1,2,[3,4]);
    my $a = (1,2,[3,4]);
    my @b = [1,2,[3,4]];
    my $b = [1,2,[3,4]];
    my @c = (1,2,(3,4));
    my $c = (1,2,(3,4));
    my @d = [1,2,(3,4)];
    my $d = [1,2,(3,4)];

    is(+@a, 3, 'Array length, nested []');
    is(+$a, 3, 'Array ref length, nested []');
    is(+@b, 3, 'Array length, nested [], outer []s');
    is(+$b, 3, 'Array ref length, nested [], outer []s');
    is(+$c, 3, 'Array ref length, nested ()');

    is(+@c, 4, 'Array length, nested ()');
    is(+@d, 4, 'Array length, nested (), outer []s');
    is(+$d, 4, 'Array ref length, nested (), outer []s');
}
