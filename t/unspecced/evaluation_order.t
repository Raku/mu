#!/usr/bin/pugs

use v6;
use Test;

plan 5;

throws_ok {
	die "first";
	die "second";
} "first", "this better work ;-)";

throws_ok {
	my @a;
	@a[die "first"] = die "second";
} "first", "evaluation order of left/right sides of assignment";

throws_ok {
	my @a = (die "first", die "second");
}"first", "evaluation order of list context";

throws_ok {
	my $x = die "first", die "second";
} "first", "evaluation order of multiple items in scalar context";

throws_ok {
	say(die "first", die "second");
} "first", "evaluation in function application";


