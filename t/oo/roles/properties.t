#!/usr/bin/pugs

use v6;
require Test;

plan 5;

# L<A12/"Use of Roles at Run Time (mixins)" /"You can declare one with"/>
todo_eval_ok 'my property answer', "basic property declaration";
my $a = 3;
is $a, 3, "basic sanity";
todo_eval_ok '$a does answer(42)', "property mixin";
todo_eval_is '$a.answer', 42,      "attribute mixin worked correctly";
todo_eval_ok '$a ~~ answer',       "var now does 'answer'";
