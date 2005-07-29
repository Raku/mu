#!/usr/bin/pugs

use v6;
use Test;

plan 1;

# Normal Pugs passes this test, but PIL2JS does not currently.
{
  my @array  = <a b c d>;
  my @result = map { () } @array;

  is +@result, 0, "map works with the map body returning empty lists";
}
