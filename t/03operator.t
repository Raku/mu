#!/usr/bin/pugs

use v6;
use PugsTest;

say 1..1;
my $str1 = "foo";
my $str2 = "bar";
my $str3 = "foobar";
my $str4 = $str1~$str2;
PugsTest.ok ("$str3 eq $str4", "operator ~")

