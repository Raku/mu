#!/usr/bin/pugs

use v6;
require Test;

plan 9;

{ # chop a string
	my $str = "foo";
	is(chop($str), "o", "o removed");
	is($str, "fo", "and isn't in the string anymore");
};

{ # chop serveral things
	my ($a, $b) = ("bar", "gorch");
	# FIXME: todo_is(eval 'chop($a, $b)', "h", "two chars removed, second returned");
	todo_is($a, "ba", "first string");
	todo_is($b, "gorc", "second string");
};

{ # chop elements of array
	my @array = ("fizz", "buzz");
	is(chop(@array), "z", "two chars removed second returned");
	todo_is(@array[0], "fiz", "first elem");
	todo_is(@array[1], "buz", "second elem");
};

{ # chop a hash
	my %hash = ( "key", "value", "other", "blah");

	# FIXME: todo_is(chop(%hash), "h"|"e", "chopping hash returns last char of either value");
	todo_is(%hash<key>, "valu", "first value chopped");
	todo_is(%hash<other>, "bla", "second value chopped");
};
