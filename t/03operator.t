#!/usr/bin/pugs

use v6;
require Test;

plan 26;

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
ok(!($five != 5), "!= (const on rhs)");

ok(5 == $five, "== (const on lhs)");
ok(!(5 != $five), "!= (const on lhs)");

ok($five == (2 + 3), "== (sum on rhs)");
# XXX: the following test doesn't parse for some reason!
ok(!($five != (2 + 3)), "== (sum on rhs)");

ok(eval '(2 + 3) == $five', "== (sum on lhs)");
ok(eval '!((2 + 3) != $five)', "== (sum on lhs)");

ok(5 > 4 > 3, "chained comparison");
ok(eval '3 < 4 < 5', "chained comparison");
ok(5 == 5 > -5, "chained comparison with equality");
ok(eval '!(3 > 4 < 5)', "chained comparison");
ok('5 <= 5 > -5', "chained comparison with <=");
ok('-5 < 5 >= 5', "chained comparison with >=");

ok("5" gt "4" gt "3", "chained str comparison");
ok("3" lt "4" lt "5", "chained str comparison");
ok(!("3" gt "4" lt "5"), "chained str comparison");
ok("5" eq "5" gt "0", "chained str comparison with equality");
ok("5" le "5" gt "0", "chained str comparison with le");
ok("0" lt "5" ge "5", "chained comparison with ge");
