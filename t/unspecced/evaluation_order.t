#!/usr/bin/pugs

use v6;
use Test;

plan 10;

throws_ok {
	die "first";
	die "second";
}, "first", "this better work ;-)";

throws_ok {
	my @a;
	@a[die "first"] = die "second";
}, "first", "evaluation order of left/right sides of array assignment";

throws_ok {
	my @a;
	@a[die "first"] := die "second";
}, "first", "evaluation order of left/right sides of array binding";

throws_ok {
        my sub foo ($arg) is rw { my $var };
        foo(die "first") = die "second";
}, "first", "evaluation order of left/right sides of lvalue sub assignment";

throws_ok {
        my sub foo ($arg) is rw { my $var };
        foo(die "first") := die "second";
}, "first", "evaluation order of left/right sides of lvalue sub binding";

throws_ok {
	my @a = (die("first"), die("second"));
}, "first", "evaluation order of list context";

throws_ok {
	my $x = die("first"), die("second");
}, "first", "evaluation order of multiple items in scalar context";

throws_ok {
	say(die("first"), die("second"));
}, "first", "evaluation in function application";

throws_ok {
        (die "first")(die "second");
}, "first", "evaluation order of using (die()) as subref";

throws_ok {
        (die "first") += die "second";
}, "first", "evaluation order of +=";
