#!/usr/bin/pugs

use v6;
require Test;

plan 2;

my $text  = "abc";
my %ret;

# Following dies untrappable.
#  pugs: cannot cast from VList [VStr "a",VStr "b",VStr "c"] to AST.VCode
eval_ok '%ret = map { $_ => uc $_ }, split "", $text', "=> works in a map block";
#  The reason is that {=>} was inlined as a hash composition expression.

# But this form works:
%ret = map { $_, uc $_ }, split "", $text;
is ~%ret.kv, "a A b B c C", "map called with function return values works";