#!/usr/bin/pugs

use v6;
require Test;

plan(7);

is ('a' x 3, 'aaa');
is ('ab' x 4, 'abababab');
is (1 x 5, '11111');
is ('' x 6, '');

my @foo = 'x' xx 10;
is (@foo[0], 'x');
is (@foo[9], 'x');
is (+@foo, 10);
