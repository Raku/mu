#!/usr/bin/pugs

use v6;
use Test;

plan 3;

if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

{
  my $contents = slurp "README";
  ok index($contents, "Pugs") != -1, "slurp() worked";
}

{
  dies_ok { slurp "does-not-exist" } "slurp() on not-existant files fails";
}

{
  dies_ok { slurp "t/" } "slurp() on directories fails";
}
