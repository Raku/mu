#!/usr/bin/pugs

use v6;
use Test;

plan 4;

{
  my %hash = (a => 1, b => 2);
  is "%hash<a>",   1, '"%hash<a>" works';
  is "<%hash<a>>", '<1>', '"<%hash<a>>" works';
}

{
  my $hash = { a => 1, b => 2 };
  is "$hash<a>",   1, '"$hash<a>" works';
  is "<$hash<a>>", '<1>', '"<$hash<a>>" works';
}
