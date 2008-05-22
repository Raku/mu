use v6;

use Test;

plan 17 + 35;

# Ambiguity tests, see thread "Ambiguity of parsing numbers with
# underscores/methods" on p6l started by Ingo Blechschmidt:
# L<"http://www.nntp.perl.org/group/perl.perl6.language/22769">
# Answer from Luke:
#   I think we should go with the method call semantics in all of the ambiguous
#   forms, mostly because "no such method: Int::e5" is clearer than silently
#   succeeding and the error coming up somewhere else.
dies_ok { 2.e123 },    "2.e123 parses as method call";
dies_ok { 2.foo  },    "2.foo  parses as method call";

is  +'00123', 123, "Leading zeroes stringify correctly";

is :16("ff"), 255, "Adverbial function form of hex number works";
is :10("99"),  99, "Adverbial function form of dec number works";
is :8("77"),   63, "Adverbial function form of oct number works";
is :2("11"),    3, "Adverbial function form of bin number works";

is :16<ff>,   255,   "Adverbial string form of hex number works";
is :10<99>,    99,   "Adverbial string form of dec number works";
is :8<77>,     63,   "Adverbial string form of oct number works";
is :2<11>,      3,   "Adverbial string form of bin number works";
is :2<1_0.1>,   2.5, "Adverbial string form can include . and _";

is :10<99*10**2>, 99e2, "Adverbial form of exponentiation works", :todo<feature>;
is :2<11*10**2>, 300, "Adverbial exponent defaults to decimal", :todo<feature>;
is :2«1.1*:2<10>**:2<10>», 6, "Adverbial form in french quotes", :todo<feature>;

is eval(":2<2>"),   undef, ":2<2> recognized as illegal", :todo<bug>;
is eval(":10<3a>"), undef, ":10<3a> recognized as illegal", :todo<bug>;

#*** Missing required parameters: $_
#    at misc.t line xx, column 13-24
for 2..36 {
    is eval(":{$_}<11>"), $_ + 1, "Adverbial form of base $_ works";
}
