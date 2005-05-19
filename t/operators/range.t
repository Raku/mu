#!/usr/bin/pugs

use v6;
use Test;

plan 7;

is ~(3..6), "3 4 5 6", "(..) works (1)";
is ~(3..3), "3",       "(..) works (2)";
is ~(3..2), "",        "(..) works (3)";

{
  my @array = 3...;
  skip 2, "Skipping hanging tests";
  # is @array[0], 3, "(...) works (1)";
  # is @array[3], 6, "(...) works (2)";
}

is ~(3..9-3), "3 4 5 6", "(..) has correct precedence (1)";
is ~(2+1..6), "3 4 5 6", "(..) has correct precedence (2)";
