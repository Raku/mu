#!/usr/bin/pugs

use v6;
require Test;

=kwid

Tests for Synopsis 3

=cut

plan 29;

my $str1 = "foo";
my $str2 = "bar";
my $str3 = "foobar";
my $str4 = $str1~$str2;

is($str3, $str4, "~");

my $bar = "";
($str3 eq $str4) ?? ($bar = 1) :: ($bar = 0);

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
ok(!($five != (2 + 3)), "== (sum on rhs)");

is(2 + 3, $five, "== (sum on lhs)");
ok((2 + 3) == 5, "== (sum on lhs)");
ok(!((2 + 3) != $five), "== (sum on lhs)");

# String Operations
is("text " ~ "stitching", "text stitching", 'concatenation with ~ operator');

# Bit Stitching

is(2 || 3, 2, "|| returns first true value");
todo_is(eval '2 ?| 3', 1, "boolean or (?|) returns 0 or 1");
ok(!(defined( 0 || undef)), "|| returns last false value of list?");
todo_is(eval '0 ?| undef', 0, "boolean or (?|) returns 0 or 1");

#junctions

ok((all((4|5|6) + 3) == one(7|8|9)), "all elements in junction are incremented");
ok((any(1..6) == one(1|2|3|4|5|6)), "any elements will match via junction");


ok( 7 > any(4..12), "any test against scalar" );


my @oldval  = (5, 8, 12);

my @newval1 = (17, 15, 14); # all greater
my @newval2 = (15, 7,  20); # some less some greater
my @newval3 = (3, 1, 4);    # all less
my @newval4 = (1,2,40);     

ok( any(@newval4) > any(@oldval), "any test array against any array" );
ok( any(@newval4) > all(@oldval), "any test array against all array" );
ok( all(@newval2) > any(@oldval), "all test array against any array" );
ok( all(@newval1) > all(@oldval), "all test array against all array" );

ok(42 > 12 & 20 & 32, "test the all infix operator");


# Hyper ops
my @rv;
eval '@rv = (1,2,3,4) >>+<< (1,2,3,4)';
todo_is("@rv[]", "2 4 6 8", 'hyper-add');
