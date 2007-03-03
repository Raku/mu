#!/usr/bin/perl -i.pre
use warnings;
use strict;

# Line numbers are not maintained during the munging
while (<>) {
  $_ = '' if /^#ifndef\s+HADDOCK\s*$/ .. /^#endif\s*$/;

  # CPP macros
  s/^#(.*)/{- $1 -}/;

  # Parallel arrays
  s/\[:/[/g;
  s/:\]/]/g;

  print "\n\n-- #ignore-exports\n" if /^module/;
  print;
}
