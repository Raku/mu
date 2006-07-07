use v6-alpha;

use Test;

# L<"http://use.perl.org/~autrijus/journal/25351">
# Roles are also classes! They can be instantiated just fine if they are
# concrete enough. Basically they mean composable classes or mixin-able
# classes. Hence, RoleName.new() instantiates an object that will probably fail
# on all stubs.

plan 3;

role SampleRole {
  method sample_method () { 42 }
}

{
  my $obj = SampleRole.new;
  ok $obj, "roles can be instantiated";

  ok $obj ~~ SampleRole, "our instantiated role object smartmatches against our role";

  is $obj.sample_method, 42, "calling a method on our instantiated role object worked";
}
