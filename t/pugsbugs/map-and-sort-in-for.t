#!/usr/bin/pugs

use v6;
use Test;

plan 3;

# works
{
  my @array = <1 2 3 4>;
  my $output;

  for map { 1 } @array -> $elem {
    $output ~= "$elem,";
  }

  is $output, "1,1,1,1,", "map works in for";
}

# works, too
{
  my @array = <1 2 3 4>;
  my $output;

  for sort @array -> $elem {
    $output ~= "$elem,";
  }

  is $output, "1,2,3,4,", "sort works in for";
}

# does not work
{
  my @array = <1 2 3 4>;
  my $output;

  for map { 1 } sort @array -> $elem {
    $output ~= "$elem,";
  }

  is $output, "1,1,1,1,", "map and sort work in for";
}
