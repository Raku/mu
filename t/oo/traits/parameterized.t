#!/usr/bin/pugs

use v6;
require Test;

plan 7;

=pod

Parameterized traits tests, see L<S12/"Traits">.

=cut

# L<S12/"Traits">
# Basic definition
todo_eval_ok 'role cool {
  has $.cool;

  multi sub trait_auxiliary:<is>(cool $trait, Any $container: $val) {
    $.cool = $val;
    $container does cool($val);
  }
', "role definition works";

my $a = 42;
is           $a, 42, "basic sanity (1)";
todo_eval_ok '$a does cool(23)',   "imperative does worked (1)";
todo_eval_is '$a.cool',      23,   "attribute was set correctly (1)";

my $b = 23;
is           $b, 23, "basic sanity (2)";
todo_eval_ok '$b does cool("hi")', "imperative does worked (2)";
todo_eval_is '$b.cool',      "hi", "attribute was set correctly (2)";
