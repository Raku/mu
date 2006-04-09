#!/usr/bin/pugs

use v6;
use Test;

# L<S02/Molecules /Multiline comments/>

plan 3;

ok .#[
    Multiline
    comments
    is fine
] 1;

ok .#(
    Parens works also
) 1;

ok .#!
    Or this...
! 1;
