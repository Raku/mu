#!/usr/bin/pugs

BEGIN {
	unless try({ eval("1", :lang<perl5>) }) {
		exit;
	}
	eval('unshift @INC, "t/perl5/TestFiles";',:lang<perl5>);
}
use v6;
use Test;
plan 1;


use perl5:Foo;

lives_ok(try({ Foo.bar() }),"Perl 5 exception (die) caught",:todo<bug>);
