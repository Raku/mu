use v6;

use Test;

plan 5;

# L<A12/"Use of Roles at Run Time (mixins)" /You can declare one with/>
ok eval('my property answer'), "basic property declaration", :todo<feature>;
my $a = 3;
is $a, 3, "basic sanity";
ok eval('$a does answer(42)'), "property mixin";
is eval('$a.answer'), 42,      "attribute mixin worked correctly", :todo<feature>;
ok eval('$a ~~ answer'),       "var now does 'answer'", :todo<feature>;
