#!/usr/bin/perl -i.pre
use warnings;
use strict;

while (<>) {
  s/^#(.*)/{- $1 -}/;
  print "\n\n-- #ignore-exports\n" if /^module/;
  print;
}
