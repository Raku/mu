use v6-alpha;

use Test;

plan 14;

# L<S12/"Roles">
{
  my $a = 3;
  is $a, 3, "basic sanity";
  ok eval('$a does role { has $.cool = "yeah" }'), "anonymous role mixin", :todo<feature>;
  is $a, 3, "still basic sanity";
  is eval('$a.cool'), "yeah", "anonymous role gave us an attribute", :todo<feature>;
}

# The same, but we story the anonymous role in a variable
{
  my $a = 3;
  is $a, 3, "basic sanity";
  my $role;
  ok eval('$role = role { has $.cool = "yeah" }'), "anonymous role definition", :todo<feature>;
  ok eval('$a does $role'), "anonymous role variable mixin";
  is $a, 3, "still basic sanity";
  is eval('$a.cool'), "yeah", "anonymous role variable gave us an attribute", :todo<feature>;
}

# Guarantee roles are really first-class-entities:
{
  ok eval('
    sub role_generator(Str $val) {
      return role {
        has $.cool = $val;
      }
    }
  '), "role generating functions defined", :todo<feature>;

  my $a = 3;
  is $a, 3, "basic sanity";
  ok eval('$a does role_generator("hi")'), "role generating function mixin";
  is $a, 3, "still basic sanity";
  is eval('$a.cool'), "hi", "role generating function gave us an attribute", :todo<feature>;
}
