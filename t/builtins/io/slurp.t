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
  my $contents = slurp "does-not-exist";
  ok !defined($contents), "slurp() on not-existant files returns undef";
}

{
  my $contents = slurp "t/";
  ok !defined($contents), "slurp() on directories returns undef";
}
