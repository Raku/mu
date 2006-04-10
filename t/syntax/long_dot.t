#!/usr/bin/pugs

use v6;
use Test;

plan 3;

is(4.       .sqrt, 2, 'long dot with numbers');
is("x".     .bytes, 1, 'long dot with strings');
is("x".     .bytes(), 1, 'long dot with strings + parens');
