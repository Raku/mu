use v6;

use Test;

plan 7;

=begin pod

Parameterized traits tests, see L<S12/"Traits">.

=end pod

# L<S12/"Traits">
# Basic definition
my $role_works;
ok eval('role cool {
  has $.cool;

  multi sub trait_auxiliary:<is>(cool $trait, Any $container; $val) {
    $.cool = $val;
    $container does cool($val);
  }
  $role_works = 1;
'), "role definition works", :todo<feature>;

unless ($role_works) {
    skip_rest 'role definition is broken'; exit;
}

my $a = 42;
is           $a, 42, "basic sanity (1)";
ok eval('$a does cool(23)'),   "imperative does worked (1)", :todo<feature>;
is eval('$a.cool'),      23,   "attribute was set correctly (1)", :todo<feature>;

my $b = 23;
is           $b, 23, "basic sanity (2)";
ok eval('$b does cool("hi")'), "imperative does worked (2)", :todo<feature>;
is eval('$b.cool'),      "hi", "attribute was set correctly (2)", :todo<feature>;
