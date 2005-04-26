#!/usr/bin/pugs

use v6;
use Test;

=head1 DESCRIPTION

This test tests for macro support. Note that much of macros isn't specced yet.

See L<A06/"Macros">.

=cut

plan 1;

skip 1, "macros not yet implemented";
exit;

=begin END
{
  my $z = 3;
  my macro returns_a_closure {
    my $x = 42;
    { 100 + $x + $z };
  }

  is returns_a_closure, 145, "closure returning macro (1)";
}
