#!/usr/bin/pugs

use v6;
use Test;

plan 7;

=pod

Parameterized traits tests, see L<S12/"Traits">.

=cut

# L<S12/"Traits">
# Basic definition
eval_ok 'role cool {
  has $.cool;

  multi sub trait_auxiliary:<is>(cool $trait, Any $container: $val) {
    $.cool = $val;
    $container does cool($val);
  }
', "role definition works", :todo<feature>;

my $a = 42;
is           $a, 42, "basic sanity (1)";
eval_ok '$a does cool(23)',   "imperative does worked (1)", :todo<feature>;
eval_is '$a.cool',      23,   "attribute was set correctly (1)", :todo<feature>;

my $b = 23;
is           $b, 23, "basic sanity (2)";
eval_ok '$b does cool("hi")', "imperative does worked (2)", :todo<feature>;
eval_is '$b.cool',      "hi", "attribute was set correctly (2)", :todo<feature>;
