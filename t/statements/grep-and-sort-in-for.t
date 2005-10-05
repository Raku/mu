#!/usr/bin/pugs

use v6;
use Test;

plan 3;

# works
{
  my @array = <1 2 3 4>;
  my $output;

  for grep { 1 } @array -> $elem {
    $output ~= "$elem,";
  }

  is $output, "1,2,3,4,", "grep and sort work in for";
}

# works, too
{
  my @array = <1 2 3 4>;
  my $output;

  for sort @array -> $elem {
    $output ~= "$elem,";
  }

  is $output, "1,2,3,4,", "grep and sort work in for";
}

{
  my @array = <1 2 3 4>;
  my $output;

  for grep { 1 } sort @array -> $elem {
    $output ~= "$elem,";
  }

  is $output, "1,2,3,4,", "grep and sort work in for";
}
