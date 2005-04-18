#!/usr/bin/pugs

use v6;
require Test;

=pod

Test that a constant list can have C<map> applied to it.

  ("foo","bar").map(){ $_.substr(1,1) }
  
should be equivalent to

  my @val = ("foo","bar");
  @val = map { substr($_,1,1) }, @val;

=cut

my @expected = ("foo","bar");
@expected = map { substr($_,1,1) }, @expected;

fail("FIXME parsefail");
#todo_eval_is( '("foo","bar").map(){ $_.substr(1,1) }', @expected );
