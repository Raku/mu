#!/usr/bin/pugs

use v6;
use Test;

plan 2;

# Undeterminate Math results
# see L<"http://mathworld.wolfram.com/Indeterminate.html">
# L<S02/"Built-In Data Types" /Perl 6 should by default make standard IEEE floating point concepts visible/>

is 0 * Inf  , NaN, "0 * Inf";
is Inf / Inf, NaN, "Inf / Inf";
is Inf - Inf, NaN, "Inf - Inf";
is 0**0     , NaN ,"0**0";
is Inf**0   , NaN, "Inf**0";
