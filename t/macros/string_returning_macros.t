#!/usr/bin/pugs

use v6;
require Test;

=head1 DESCRIPTION

This test tests for basic macro support. Note that much of macros isn't specced
yet.

See L<A06/"Macros">.

=cut

plan 4;

skip 4, "macros not yet implemented";
exit;

=begin END
{
  macro dollar_foo { '$foo' }
  my $foo = 42;
  is dollar_foo, $foo, "simple string returning macro (1)";
  dollar_foo = 23;
  is $foo, 23, "simple string returning macro (2)";
}

{
  macro plus_3 { '+ 3' }
  my $foo = 42;
  is $foo plus_3, 45, "simple string returning macro (3)";
}

{
  macro four { '4' }
  my $foo = 100 + four;
  is $foo, 104, "simple string returning macro (4)";
}
