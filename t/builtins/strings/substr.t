#!/usr/bin/pugs

use v6;
use Test;

plan 31;

# L<S29/"Perl6::Str" /substr/>

{ # read only
    my $str = "foobar";

    is(substr($str, 0, 1), "f", "first char");
    is(substr($str, -1), "r", "last char");
    is(substr($str, 1, 2), "oo", "arbitrary middle");
    is(substr($str, 3), "bar", "length omitted");
    is(substr($str, 3, 10), "bar", "length goes past end");
    is(substr($str, 20, 5), undef, "substr outside of string");
    is(substr($str, -100, 10), undef, "... on the negative side");

    is(substr($str, 0, -2), "foob", "from beginning, with negative length");
    is(substr($str, 2, -2), "ob", "in middle, with negative length");
    is(substr($str, 3, -3), "", "negative length - gives empty string");

    is($str, "foobar", "original string still not changed");
};

skip 4, "more discussion needed";

=begin more-discussion-needed

{ # replacement
    my $str = "foobar";

    substr($str, 2, 1, "i");
    is($str, "foibar", "fourth arg to substr replaced part");

    substr($str, -1, 1, "blah");
    is($str, "foibablah", "longer replacement expands string");

    substr($str, 1, 3, "");
    is($str, "fablah", "shorter replacement shrunk it");

    substr($str, 1, -1, "aye");
    is($str, "fayeh", "replacement with negative length");
};

=end more-discussion-needed

=cut

# as lvalue, XXX: not sure this should work, as that'd be action at distance:
#   my $substr = \substr($str, ...);
#   ...;
#   some_func $substr; # manipulates $substr
#   # $str altered!
# But one could think that's the wanted behaviour, so I leave the test in.
{
    my $str = "gorch ding";

    substr($str, 0, 5) = "gloop";
    is($str, "gloop ding", "lvalue assignment modified original string");

    my $r = \substr($str, 0, 5);
    ok(ref($r), '$r is a reference');
    is($$r, "gloop", '$r referent is eq to the substring');

    $$r = "boing";
    is($str, "boing ding", "assignment to reference modifies original", :todo);
    is($$r, "boing", '$r is consistent');

    my $o = \substr($str, 3, 2);
    is($$o, "ng", "other ref to other lvalue", :todo);
    $$r = "foo";
    is($str, "foo ding", "lvalue ref size varies but still works", :todo);
    is($$o, " d", "other lvalue wiggled around", :todo);
};

{ # as lvalue, should work
    my $str = "gorch ding";

    substr($str, 0, 5) = "gloop";
    is($str, "gloop ding", "lvalue assignment modified original string");
};

{ # as lvalue, using :=, should work
    my $str = "gorch ding";

    substr($str, 0, 5) = "gloop";
    is($str, "gloop ding", "lvalue assignment modified original string");

    my $r := substr($str, 0, 5);
    is($r, "gloop", 'bound $r is eq to the substring');

    $r = "boing";
    is($str, "boing ding", "assignment to bound var modifies original");
    is($r, "boing", 'bound $r is consistent', :todo<bug>);

    my $o := substr($str, 3, 2);
    is($o, "ng", "other bound var to other lvalue");
    $r = "foo";
    is($str, "foo ding", "lvalue ref size varies but still works");
    is($o, " d", "other lvalue wiggled around", :todo<bug>);
};

{ 
# from L<S09/"Junctions" /Each of the resulting set of calls is then recursively autothreaded/>
# See also t/junctions/s09eg.t
# This test is not working as-is
#    eval_is('substr("camel", 0|1, 2&3)', (("ca"|"am") & ("cam"|"ame")), "junctive substr", :todo);
}
