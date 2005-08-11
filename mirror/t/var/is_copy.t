#!/usr/bin/pugs

use v6;
use Test;

plan 3;

{
  sub foo($a is copy) {
    $a = 42;
    return 19;
  }

  my $bar = 23;
  is $bar,      23, "basic sanity";
  is foo($bar), 19, "calling a sub with an is copy param";
  is $bar,      23, "sub did not change our variable";
}
