#!/usr/bin/perl -i.pre
use warnings;
use strict;

print "-- #ignore-exports";

while (<>) {
  s/^#(.*)/{- $1 -}/;
  print;
}
