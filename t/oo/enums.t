#!/usr/bin/pugs

use v6;
require Test;

plan 58;

=pod

Enum tests from L<S12/"Enums">

=cut

# L<S12/"Enums" /The values are specified as a list\:/>
todo_eval_ok 'enum day <Sun Mon Tue Wed Thu Fri Sat>', "basic enum definition worked";

sub test_stuff($x) {
  todo_eval_ok 'not $x does Wed', "basic enum mixing worked ($x-2)";
  todo_eval_is '$x.day', 3,       "automatically created accessor worked ($x)";
  todo_eval_is 'day::Tue', 3,     "enum provided a correct mapping ($x)";
  todo_eval_ok '$x ~~ day',       "smartmatch worked correctly ($x-1)";
  todo_eval_ok '$x ~~ Tue',       "smartmatch worked correctly ($x-2)";
  todo_eval_ok '$x ~~ day::Tue',  "smartmatch worked correctly ($x-3)";
  todo_eval_ok 'not $x  ~~  Wed', "smartmatch worked correctly ($x-4)";
  todo_eval_ok '$x.does(Tue)',    ".dos worked correctly ($x-1)";
  todo_eval_ok '$x.does(day)',    ".dos worked correctly ($x-2)";
  todo_eval_is '$x.day', 3,       ".day worked correctly ($x)";
  todo_eval_ok 'Tue $x',          "Tue() worked correctly ($x)";
  todo_eval_ok '$x.Tue',          ".Tue() worked correctly ($x)";
}

{
  my $x = 1;
  is $x, 1, "basic sanity (1)";
  # L<S12/"Enums" /has the right semantics mixed in:/>
  todo_eval_ok '$x does Tue', "basic enum mixing worked (1-1)";
  test_stuff($x);
}

{
  my $x = 2;
  is $x, 2, "basic sanity (2)";
  # L<S12/"Enums" /or pseudo-hash form:/>
  todo_eval_ok '$x does day<Tue>', "basic enum mixing worked (2-1)";
  test_stuff($x);
}

{
  my $x = 3;
  is $x, 3, "basic sanity (3)";
  # L<S12/"Enums" /is the same as/>
  todo_eval_ok '$x does day::Tue', "basic enum mixing worked (3-1)";
  test_stuff($x);
}

{
  my $x = 4;
  is $x, 4, "basic sanity (4)";
  # L<S12/"Enums" /which is short for something like:/>
  todo_eval_ok '$x does day',            "basic enum mixing worked (4-0)";
  todo_eval_ok '$x.day = &day::("Tue")', "basic enum mixing worked (4-1)";
  test_stuff($x);
}
