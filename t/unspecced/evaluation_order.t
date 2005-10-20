#!/usr/bin/pugs

use v6;
use Test;

plan 17;

throws_ok {
	die "first";
	die "second";
}, "first", "this better work ;-)";

throws_ok {
        (die "first")[die "second"];
}, "first", "evaluation order of array access";

throws_ok {
        (die "first"){die "second"};
}, "first", "evaluation order of hash access";

throws_ok {
	my @a;
	@a[die "first"] = die "second";
}, "first", "evaluation order of left/right sides of array assignment";

throws_ok {
	my @a;
	@a[die "first"] := die "second";
}, "first", "evaluation order of left/right sides of array binding", :todo<bug>;

throws_ok {
        my sub foo ($arg) is rw { my $var };
        foo(die "first") = die "second";
}, "first", "evaluation order of left/right sides of lvalue sub assignment (1)";

throws_ok {
        my sub foo ($arg) is rw { my $var };
        foo(die "first") := die "second";
}, "first", "evaluation order of left/right sides of lvalue sub binding (1)", :todo<bug>;

throws_ok {
        my sub foo ($arg) is rw { die "second"; my $var };
        foo(die "first") = die "third";
}, "first", "evaluation order of left/right sides of lvalue sub assignment (2)";

throws_ok {
        my sub foo ($arg) is rw { die "second"; my $var };
        foo(die "first") := die "third";
}, "first", "evaluation order of left/right sides of lvalue sub binding (2)", :todo<bug>;

throws_ok {
	my @a = (die("first"), die("second"));
}, "first", "evaluation order of list context (provided by assigning to an array)";

throws_ok {
	my $a = [die("first"), die("second")];
}, "first", "evaluation order of list context (provided by creating an arrayref)";

throws_ok {
	my %a = (die("first"), die("second"));
}, "first", "evaluation order of list context (provided by assigning to a hash)";

throws_ok {
	my $x = die("first"), die("second");
}, "first", "evaluation order of multiple items in scalar context";

throws_ok {
	say(die("first"), die("second"));
}, "first", "evaluation in function application";

throws_ok {
        die("first").say(die "second");
}, "first", "evaluation in method application";

throws_ok {
        (die "first")(die "second");
}, "first", "evaluation order of using (die()) as subref";

throws_ok {
        (die "first") += die "second";
}, "first", "evaluation order of +=";
