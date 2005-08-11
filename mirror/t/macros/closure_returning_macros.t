#!/usr/bin/pugs

use v6;
use Test;

=head1 DESCRIPTION

This test tests for macro support. Note that much of macros isn't specced yet.

See L<A06/"Macros">.

=cut

plan 4;

{
  my $z = 3;
  my $in_macro;
  my $in_macro_clos;
  macro returns_a_closure {
    my $x = 42;
    $in_macro++;
    return { $in_macro_clos++; 100 + $x + $z };
  }

  is $in_macro,           1, "macro was executed during compile time";
  ok !$in_macro_clos,        "macro closure was not executed during compile time";
  is returns_a_closure, 145, "closure returning macro";
  is $in_macro_clos,      1, "macro closure was executed during runtime";
}
