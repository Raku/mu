#!/usr/bin/pugs

use v6;
use Test;

=head1 DESCRIPTION

This test tests the C<type> builtin.

Reference:
L<http://groups.google.com/groups?selm=420DB295.3000902%40conway.org>

=cut

plan 7;

# Basic subroutine/method form tests for C<type>.
{
  my $a = 3;
  eval_ok 'type $a =:= Int', "subroutine form of type", :todo(1);
  eval_ok '$a.type =:= Int', "method form of type", :todo(1);
}

# Now testing basic correct inheritance.
{
  my $a = 3;
  eval_ok '$a.type ~~ Num',    "an Int isa Num", :todo(1);
  eval_ok '$a.type ~~ Object', "an Int isa Object", :todo(1);
}

# And a quick test for Code:
{
  my $a = sub ($x) { 100 + $x };
  eval_ok '$a.type =:= Sub',    "a sub's type is Sub", :todo(1);
  eval_ok '$a.type ~~ Routine', "a sub isa Routine", :todo(1);
  eval_ok '$a.type ~~ Code',    "a sub isa Code", :todo(1);
}
