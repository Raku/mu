#!/usr/bin/pugs

use v6;
require Test;

plan 2;

my $str1 = "foo";
my $str2 = "bar";
my $str3 = "foobar";
my $str4 = $str1~$str2;

is($str3, $str4, "~");

my $bar = "";
($str3 eq $str4) ?? $bar = 1 :: $bar = 0;

ok($bar, "?? ::");

my $five = 5;
my $four = 4;
my $wibble = 4;

ok(!($five == $four), "== (false)");
ok($wibble == $four, "== (true)");
ok(!($wibble != $four), "== (false)");
ok($five != $four, "!= (true)");

ok($five == 5, "== (const on rhs)");
ok(5 == $five, "== (const on lhs)");
ok($five == (2 + 3), "== (sum on rhs)");
ok((2 + 3) == $five, "== (sum on lhs)");
