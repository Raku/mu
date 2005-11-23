#!/usr/bin/pugs

use v6;
use Test;

=pod

_ should be allowed in numbers

But according to L<S02/Literals>, only between two digits.

=cut

plan 63;

is 1_0, 10, "Single embedded underscore works";

isnt eval('1__0'), 10, "Multiple embedded underscores fail";

isnt eval('10_'), 10, "Trailing underscore fails";

isnt eval('10_.0'), 10, "Underscore before . fails";

is 3.1_41, 3.141, "Underscores work with floating point after decimal";

isnt 10_0.8, 100.8, "Underscores work with floating point before decimal";

is 0xdead_beef, 0xdeadbeef, "Underscores work with hex";

is 0b1101_1110_1010_1101_1011_1111_0110_1000, 0xdeadbeef, "Underscores work with binary";

is 2e0_1, 20, "Underscores work in the argument for e";

# Ambiguity tests, see thread "Ambiguity of parsing numbers with
# underscores/methods" on p6l started by Ingo Blechschmidt:
# L<"http://www.nntp.perl.org/group/perl.perl6.language/22769">
# Answer from Luke:
#   I think we should go with the method call semantics in all of the ambiguous
#   forms, mostly because "no such method: Int::e5" is clearer than silently
#   succeeding and the error coming up somewhere else.
dies_ok { 2.e123 },    "2.e123 parses as method call";
dies_ok { 2._foo },    "2._foo parses as method call";
dies_ok { 2._123 },    "2._123 parses as method call";
is      2.1_23, 2.123, "2.1_23 parses as number";
dies_ok { 2._e23 },    "2._23  parses as method call";
dies_ok { 2.foo  },    "2.foo  parses as method call";

is  +'00123', 123, "Leading zeroes stringify correctly";

# (Note, when this works, go fix hex.t to use :16().)

is eval(':16("ff")'), 255, "Adverbial function form of hex number works";
is eval(':10("99")'), 99, "Adverbial function form of dec number works";
is eval(':8("77")'), 63, "Adverbial function form of oct number works";
is eval(':2("11")'), 3, "Adverbial function form of dec number works";

is eval(':16<ff>'), 255, "Adverbial string form of hex number works";
is eval(':10<99>'), 99, "Adverbial string form of dec number works";
is eval(':8<77>'), 63, "Adverbial string form of oct number works";
is eval(':2<11>'), 3, "Adverbial string form of dec number works";

is eval(':10<99*10**2>'), 99e2, "Adverbial form of exponentiation works";
is eval(':2<11*10**2>'), 300, "Adverbial exponent defaults to decimal";

for 2..36 {
    is eval(":{$_}<11>"), $_ + 1, "Adverbial form of base $_ works";
}

is eval(':100[10,10]'), 1010, "Adverbial form of base 100 integer works";
is eval(":100[10,'.',10]"), 10.10, "Adverbial form of base 100 fraction works";
