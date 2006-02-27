#!/usr/bin/perl -i.pre
use warnings;
use strict;

# Line numbers are not maintained during the munging
while (<>) {
  s/^#(.*)/{- $1 -}/;
  print "\n\n-- #ignore-exports\n" if /^module/;
  print;
}
