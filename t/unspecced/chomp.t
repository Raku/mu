#!/usr/bin/pugs

use v6;
require Test;

plan 2;

my $str = "foo blah\n";
is(chomp($str), "\n", "removed char is \\n");
is($str, "foo blah", "chomp removed the \\n from the string");

