#!/usr/bin/pugs

use v6;
use Test;

my @tests = (
  ("require t::packages::RequireAndUse1",    { $^a == 42 }) xx 2,
  ("use     t::packages::RequireAndUse1",    { $^a == 42 }) xx 2,
  ("require 't/packages/RequireAndUse1.pm'", { $^a != 23 }) xx 2,
  ("require t::packages::RequireAndUse2",    { $^a != 23 }) xx 2,
  ("use     t::packages::RequireAndUse2",    { $^a != 23 }) xx 2,
  ("require 't/packages/RequireAndUse2.pm'", { $^a != 23 }) xx 2,
  ("require t::packages::RequireAndUse3",    { $^a != 23 }) xx 2,
  ("use     t::packages::RequireAndUse3",    { $^a != 23 }) xx 2,
  ("require 't/packages/RequireAndUse3.pm'", { $^a != 23 }) xx 2,
);

plan +@tests / 2;

for @tests -> $str, $expected_ret {
  state $i = 1;
  diag $str;
  my $retval = try { eval $str };
  ok defined($retval) && $retval != -1 && $expected_ret($retval),
    "require or use's return value was correct ({$i++})";
}
