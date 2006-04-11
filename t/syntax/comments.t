#!/usr/bin/pugs

use v6;
use Test;

# L<S02/Molecules /Multiline comments/>

plan 6;

ok #[
    Multiline
    comments
    is fine
] 1;

ok #(
    Parens works also
) 1;

ok #(
    (Nested parens) works also
) 1;

ok #<<<
    Or this <also> works...
>>> 1;

ok #<<<
    Even <this> <<< also >>> works...
>>> 1;

ok
#<this is special cased
1;
