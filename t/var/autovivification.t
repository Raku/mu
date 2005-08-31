#!/usr/bin/pugs

use v6;
use Test;

plan 20;

# Simple hash autovivification
{
  my $hashref;
  ok !$hashref.isa(Hash), "uninitialized variable is not a Hash (1)";

  $hashref<key> = 23;
  ok $hashref.isa(Hash), "uninitialized variable was autovivified to a hash (1)";
  is $hashref<key>,  23, "hash element assignment worked";
}

{
  my $hashref;
  ok !$hashref.isa(Hash), "uninitialized variable is not a Hash (2)";

  lives_ok { my $elem = $hashref<key> },
    "accessing a not existing hash element of an uninitialized variable works";
  ok $hashref.isa(Hash), "uninitialized variable was autovivified to a hash (2)";
}

# Simple array autovivification
{
  my $arrayref;
  ok !$arrayref.isa(Array), "uninitialized variable is not an Array (1)";

  $arrayref[42] = 23;
  ok $arrayref.isa(Array), "uninitialized variable was autovivified to an array (1)";
  is $arrayref[42],    23, "array element assignment worked";
}

{
  my $arrayref;
  ok !$arrayref.isa(Array), "uninitialized variable is not an Array (2)";

  lives_ok { my $elem = $arrayref[42] },
    "accessing a not existing array element of an uninitialized variable works";
  ok $arrayref.isa(Array), "uninitialized variable was autovivified to an array (2)";
}

# Autovivification of an array/hash element
{
  my @array;

  @array[42][23] = 17;
  is @array[42][23], 17, "autovivification of an array element to an arrayref worked";
}

{
  my @array;

  @array[42]<key> = 17;
  is @array[42]<key>, 17, "autovivification of an array element to a hashref worked";
}

{
  my %hash;

  %hash<key>[42] = 17;
  is %hash<key>[42], 17, "autovivification of a hash element to an arrayref worked";
}

{
  my %hash;

  %hash<key><innerkey> = 17;
  is %hash<key><innerkey>, 17, "autovivification of a hash element to a hashref worked";
}

# Autovification by push, unshift, etc.
{
  my $arrayref;

  push $arrayref, 1,2,3;
  is ~$arrayref, "1 2 3", "autovivification to an array by &push";
}

{
  my $arrayref;

  unshift $arrayref, 1,2,3;
  is ~$arrayref, "1 2 3", "autovivification to an array by &unshift";
}

# Autovification by push, unshift, etc. of an array/hash element
{
  my @array;

  push @array[2], 1,2,3;
  is ~@array, "  1 2 3", "autovivification of an array element to an array by &push";
}

{
  my %hash;

  push %hash<key>, 1,2,3;
  is ~%hash, "key\t1 2 3\n", "autovivification of an hash element to an array by &push";
}
