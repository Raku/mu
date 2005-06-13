#!/usr/bin/pugs

use v6;
use Test;

plan 1;

my $was_in_sub;
eval '
  my &foo = -> $arg { $was_in_sub = $arg };
  foo 42;
';

is $was_in_sub, 42, 'calling a lexically defined my()-code var worked', :todo<bug>;
