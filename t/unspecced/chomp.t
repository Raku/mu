#!/usr/bin/pugs

use v6;
require Test;

plan 4;

my $str = "foo blah\n";
is(chomp($str), "\n", "removed char is \\n");
is($str, "foo blah", "chomp removed the \\n from the string");

my $str2 = "foo blah";
ok(not defined chomp($str2), "there's no \\n to remove");
is($str2, "foo blah", "chomp didn't remove a non-\\n-char");
