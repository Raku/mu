#!/usr/bin/pugs

use v6;
use Test;

plan 9;

{ # chop a string
    my $str = "foo";
    is(chop($str), "o", "o removed");
    is($str, "fo", "and isn't in the string anymore");
};

{ # chop serveral things
    my ($a, $b) = ("bar", "gorch");
    # FIXME: is(eval 'chop($a, $b)', "h", "two chars removed, second returned", :todo);
    is($a, "ba", "first string", :todo);
    is($b, "gorc", "second string", :todo);
};

{ # chop elements of array
    my @array = ("fizz", "buzz");
    is(chop(@array), "z", "two chars removed second returned");
    is(@array[0], "fiz", "first elem", :todo);
    is(@array[1], "buz", "second elem", :todo);
};

{ # chop a hash
    my %hash = ( "key", "value", "other", "blah");

    # FIXME: is(chop(%hash), "h"|"e", "chopping hash returns last char of either value", :todo);
    is(%hash<key>, "valu", "first value chopped", :todo);
    is(%hash<other>, "bla", "second value chopped", :todo);
};
