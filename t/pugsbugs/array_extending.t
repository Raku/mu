#!/usr/bin/pugs

use v6;
require Test;

plan 4;

# Compare with Perl 5:
#   $ perl -we '
#     my @array = qw<a b c>;
#     my $foo = $array[100];
#     print exists $array[30] ? "exists" : "does not exist"
#   '
#   does not exists
my @array = <a b c d>;
is +@array, 4, "basic sanity";
my $foo = @array[20];
is +@array, 4,
  "accessing a not existing array element does not automatically extends the array";

# Reset.
@array = <a b c d>;
@array[20] = 42;
isnt +@array, 21,
  "creating an array element does automatically extend the array (1)";
isnt @array.exists(20), 1,
  "creating an array element does automatically extend the array (2)";
