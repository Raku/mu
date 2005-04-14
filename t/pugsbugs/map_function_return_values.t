#!/usr/bin/pugs

use v6;
require Test;

plan 1;

my $text  = "abc";
my %ret;

# Following dies untrappable.
#  pugs: cannot cast from VList [VStr "a",VStr "b",VStr "c"] to AST.VCode
#  %ret = map { $_ => uc $_ }, split "", $text;

is ~%ret.kv, "a A b B c C", "map called with function return values doesn't work";
