#!/usr/bin/pugs

use v6;
use PugsTest;

say "1..2";
my $str1 = "foo";
my $str2 = "bar";
my $str3 = "foobar";
my $str4 = $str1~$str2;

#PugsTest.ok("$str3 eq $str4", "operator ~");
if ($str3 eq $str4) {say "ok 1"} else {say "not ok 1"}

my $bar = "";
eval ' ($str3 eq $str4) ?? { $bar = "true" } :: { $bar = "false" } ';

if ($bar) { say "ok 2" } else { say "not ok 2 # TODO Trinary operator" }
