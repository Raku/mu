#!/usr/bin/perl -i.pre -p
use warnings;
use strict;

while (<>) {
  s/^#(.*)/{- $1 -}/;
  print;
}
