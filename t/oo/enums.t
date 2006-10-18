use v6-alpha;

use Test;

plan 58;

=pod

Enum tests from L<S12/"Enums">

=cut

# L<S12/"Enums"/values are specified as a list>
ok eval('enum day <Sun Mon Tue Wed Thu Fri Sat>'), "basic enum definition worked", :todo<feature>;

sub test_stuff($x) {
  ok eval('not $x does Wed'), "basic enum mixing worked ($x-2)", :todo<feature>;
  is eval('$x.day'), 3,       "automatically created accessor worked ($x)", :todo<feature>;
  is eval('day::Tue'), 3,     "enum provided a correct mapping ($x)", :todo<feature>;
  ok eval('$x ~~ day'),       "smartmatch worked correctly ($x-1)", :todo<feature>;
  ok eval('$x ~~ Tue'),       "smartmatch worked correctly ($x-2)", :todo<feature>;
  ok eval('$x ~~ day::Tue'),  "smartmatch worked correctly ($x-3)", :todo<feature>;
  ok eval('not $x  ~~  Wed'), "smartmatch worked correctly ($x-4)", :todo<feature>;
  ok eval('$x.does(Tue)'),    ".dos worked correctly ($x-1)", :todo<feature>;
  ok eval('$x.does(day)'),    ".dos worked correctly ($x-2)", :todo<feature>;
  is eval('$x.day'), 3,       ".day worked correctly ($x)", :todo<feature>;
  ok eval('Tue $x'),          "Tue() worked correctly ($x)", :todo<feature>;
  ok eval('$x.Tue'),          ".Tue() worked correctly ($x)", :todo<feature>;
}

{
  my $x = 1;
  is $x, 1, "basic sanity (1)";
  # L<S12/"Enums" /has the right semantics mixed in:/>
  ok eval('$x does Tue'), "basic enum mixing worked (1-1)";
  test_stuff($x);
}

{
  my $x = 2;
  is $x, 2, "basic sanity (2)";
  # L<S12/"Enums" /or pseudo-hash form:/>
  ok eval('$x does day<Tue>'), "basic enum mixing worked (2-1)";
  test_stuff($x);
}

{
  my $x = 3;
  is $x, 3, "basic sanity (3)";
  # L<S12/"Enums" /is the same as/>
  ok eval('$x does day::Tue'), "basic enum mixing worked (3-1)";
  test_stuff($x);
}

{
  my $x = 4;
  is $x, 4, "basic sanity (4)";
  # L<S12/"Enums" /which is short for something like:/>
  ok eval('$x does day'),            "basic enum mixing worked (4-0)";
  ok eval('$x.day = &day::("Tue")'), "basic enum mixing worked (4-1)", :todo<feature>;
  test_stuff($x);
}
