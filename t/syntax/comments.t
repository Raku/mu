#!/usr/bin/pugs

use v6;
use Test;

# L<S02/Molecules /Multiline comments/>

plan 1;

ok .#[
    Multiline
    comments
    is fine
] 1;

