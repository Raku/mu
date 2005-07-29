#!/usr/bin/pugs

use v6;
use Test;

plan 5;

{
  my @array = <a b c d>;
  is ~(try { @array.map:{ $_ ~= "c"; $_ ~ "d" } }), "acd bcd ccd dcd",
    'mutating $_ in map works (1)';
  is ~@array, "ac bc cc dc",
    'mutating $_ in map works (2)';
}

{
  my @array = <a b c d>;
  is ~(try { @array.grep:{ $_ ~= "c"; 1 } }), "ac bc cc dc",
    'mutating $_ in grep works (1)';
  is ~@array, "ac bc cc dc",
    'mutating $_ in grep works (2)';
}

{
  my @array = <a b c d>;
  for @array { $_ ~= "c" }
  is ~@array, "ac bc cc dc",
    'mutating $_ in for works';
}

# Hm... what about @array.sort:{ $_ = ... }? Disallow? (As that would prevent
# many optimizations...)
