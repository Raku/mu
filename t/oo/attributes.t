#!/usr/bin/pugs

use v6;
require Test;

plan 10;

=pod

Very basic class attributes tests from L<S12/"Attributes">

=cut

# L<S12/"Attributes">
eval 'class Foo {
  has $.tail is rw;
  has @.legs;
  has $:brain;

  sub set_legs  (@legs) { @.legs = @legs }
  sub inc_brain ()      { $:brain++ }
  sub get_brain ()      { $:brain }
}';

my $foo = eval 'Foo.new()';

todo_eval_is '$foo.tail = "a"', "a", "setting a public rw attribute";
todo_eval_is '$foo.tail',       "a", "getting a public rw attribute";

todo_eval_ok '$foo.set_legs(1,2,3)',       "setting a public ro attribute (1)";
todo_eval_is '$foo.legs.[1]', 2,           "getting a public ro attribute (1)";
# ok instead of todo_ok to suppress "unexpected succeeded"-messages
ok           !eval('$foo.legs = (4,5,6)'), "setting a public ro attribute (2)";
todo_eval_is '$foo.legs.[1]', 2,           "getting a public ro attribute (2)";

todo_eval_ok '$foo.inc_brain()',  "modifiying a private attribute (1)";
todo_eval_is '$foo.get_brain', 1, "getting a private attribute (1)";
todo_eval_ok '$foo.inc_brain()',  "modifiying a private attribute (2)";
todo_eval_is '$foo.get_brain', 2, "getting a private attribute (2)";
