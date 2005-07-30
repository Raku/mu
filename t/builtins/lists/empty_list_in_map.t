#!/usr/bin/pugs

use v6;
use Test;

plan 3;

# Normal Pugs passes this test, but PIL2JS does not currently.
{
  my @array  = <a b c d>;
  my @result = map { () } @array;

  is +@result, 0, "map works with the map body returning empty lists";
}

{
  my @array  = <a b c d>;
  my @result = map { [] } @array;

  is +@result, 4, "map works with the map body returning an empty arrayref";
}

{
  my @array  = <a b c d>;
  my @result = map { undef } @array;

  is +@result, 4, "map works with the map body returning undef";
}
