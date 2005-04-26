#!/usr/bin/pugs

use v6;
use Test;

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

    ok(+@a == 3, 'Array length, nested []');
    ok(+$a == 3, 'Array ref length, nested []');
    ok(+@b == 3, 'Array length, nested [], outer []s');
    ok(+$b == 3, 'Array ref length, nested [], outer []s');

    ok(+@c == 4, 'Array length, nested ()');
    ok(+$c == 4, 'Array ref length, nested ()');
    ok(+@d == 4, 'Array length, nested (), outer []s');
    ok(+$d == 4, 'Array ref length, nested (), outer []s');
}
