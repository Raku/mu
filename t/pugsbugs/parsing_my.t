#!/usr/bin/pugs

use v6;
require Test;

=pod

The parser has difficulties with chained my statements.

  my $x = my $y = 0;
  No compatible subroutine found: &my
  App "&my" [] [Syn "=" [Var "$y",Val (VInt 0)]]

=cut

plan 1;

ok(eval('my $x = my $y = 0; 1'), 'my $x = my $y = 0');

