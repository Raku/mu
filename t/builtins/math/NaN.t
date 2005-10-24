#!/usr/bin/pugs

use v6;
use Test;

plan 2;

# Undeterminate Math results
# see http://mathworld.wolfram.com/Indeterminate.html

is 0 * Inf  , NaN, "0 * Inf";
is Inf / Inf, NaN, "Inf / Inf";
is Inf - Inf, NaN, "Inf - Inf";
is 0**0     , NaN ,"0**0";
is Inf**0   , NaN, "Inf**0";