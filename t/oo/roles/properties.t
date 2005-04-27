#!/usr/bin/pugs

use v6;
use Test;

plan 5;

# L<A12/"Use of Roles at Run Time (mixins)" /You can declare one with/>
eval_ok 'my property answer', "basic property declaration", :todo;
my $a = 3;
is $a, 3, "basic sanity";
eval_ok '$a does answer(42)', "property mixin", :todo;
eval_is '$a.answer', 42,      "attribute mixin worked correctly", :todo;
eval_ok '$a ~~ answer',       "var now does 'answer'", :todo;
