#!/usr/bin/pugs

use v6;
use Test;

plan 1;

my $text  = "abc";
my %ret;

# Following dies untrappable.
#  pugs: cannot cast from VList [VStr "a",VStr "b",VStr "c"] to AST.VCode
#  %ret = map { $_ => uc $_ }, split "", $text;
#  The reason is that {=>} was inlined as a hash composition expression.

# But this form works:
%ret = map { $_, uc $_ }, split "", $text;
is ~%ret.kv, "a A b B c C", "map called with function return values doesn't work";
