#!/usr/bin/pugs

use v6;
require Test;

# These tests are based on the behavior of blead-perl as of 2005-03-06

{ # read only
	my $str = "foobar";

	todo_is(eval 'substr($str, 0, 1)', "f", "first char");
	todo_is(eval 'substr($str, -1)', "r", "last char");
	todo_is(eval 'substr($str, 1, 2)', "oo", "arbitrary middle");
	todo_is(eval 'substr($str, 3)', "bar", "length omitted");
	todo_is(eval 'substr($str, 3, 10)', "bar", "length goes past end");
	is(eval 'substr($str, 20, 5)', undef, "substr outside of string");
	is(eval 'substr($str, -100, 10)', undef, "... on the negative side");

	is($str, "foobar", "original string still not changed");
};

{ # replacement
	my $str = "foobar";

	eval 'substr($str, 2, 1, "i")';
	todo_is($str, "foibar", "fourth arg to substr replaced part");

	eval 'substr($str, -1, 1, "blah")';
	todo_is($str, "foibablah", "longer replacement expands string");

	eval 'substr($str, 1, 3, "")';
	todo_is($str, "fablah", "shorter replacement shrunk it");
};

{ # as lvalue
	my $str = "gorch ding";

	eval 'substr($str, 0, 5) = "gloop"';
	todo_is($str, "gloop ding", "lvalue assignment modified original string");
	
	my $r;
	eval '$r = \substr($str, 0, 5)';
	todo_ok(ref($r), '$r is a reference');
	todo_is(eval '$$r', "gloop", '$r referent is eq to the substring');

	eval '$$r = "boing"';
	todo_is($str, "boing ding", "assignment to reference modifies original");
	todo_is(eval '$$r', "boing", '$r is consistent');

	my $o;
	eval '$o = \substr($str, 3, 2)';
	todo_is(eval '$$o', "ng", "other ref to other lvalue");
	eval '$$r = "foo"';
	todo_is($str, "foo ding", "lvalue ref size varies but still works");
	todo_is(eval '$$o', " d", "other lvalue wiggled around");
};

{ # from synopsis 9
	todo_is(eval 'substr("camel", 0|1, 2&3)', (("ca"|"am") & ("cam"|"ame")), "junctive substr");
}
