#!/usr/bin/pugs

use v6;
require Test;

my $str1 = "foo";
my $str2 = "bar";
my $str3 = "foobar";
my $str4 = $str1~$str2;

is($str3, $str4, "~");

my $bar = "";
($str3 eq $str4) ?? $bar = 1 :: $bar = 0;

ok($bar, "?? ::")
