#!/usr/bin/pugs

use v6;
use Test;

plan 5;

# L<S04/"The Relationship of Blocks and Declarations" /function has been renamed/>
{
  my $a = 42;
  {
    temp $a = 23;
    is $a, 23, "temp() changed the variable (1)";
  }
  is $a, 42, "temp() restored the variable (1)";
}

# Test that temp() restores the variable at scope exit, not at subroutine
# entry.
{
  my $a     = 42;
  my $get_a = { $a };
  {
    temp $a = 23;
    is $a,       23, "temp() changed the variable (2-1)";
    is $get_a(), 23, "temp() changed the variable (2-2)";
  }
  is $a, 42, "temp() restored the variable (2)";
}
