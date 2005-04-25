#!/usr/bin/pugs

use v6;
require Test;

plan 4;

my $text  = "abc";
my %ret;

%ret = map { $_ => uc $_; } split "", $text;
is ~%ret.kv, "a A b B c C", "=> works in a map block";

%ret = map { $_ => uc $_; }, split "", $text;
is ~%ret.kv, "a A b B c C", "=> works in a map block";

%ret = map { $_, uc $_ } split "", $text;
is ~%ret.kv, "a A b B c C", "map called with function return values works";

%ret = map { $_, uc $_ }, split "", $text;
is ~%ret.kv, "a A b B c C", "map called with function return values works";
