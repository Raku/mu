#!/usr/bin/pugs

use v6;
use Test;

plan 5;

# Undeterminate Math results
# see L<"http://mathworld.wolfram.com/Indeterminate.html">
# L<S02/"Built-In Data Types" /Perl 6 should by default make standard IEEE floating point concepts visible/>

is 0 * Inf  , NaN, "0 * Inf";
is Inf / Inf, NaN, "Inf / Inf";
is Inf - Inf, NaN, "Inf - Inf";

# XXX - the semantics below are unspecified.
is 0**0     , 1, "0**0 is 1, _not_ NaN";
is Inf**0   , 1, "Inf**0 is 1, _not_ NaN";
