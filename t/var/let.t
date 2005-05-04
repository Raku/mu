#!/usr/bin/pugs

use v6;
use Test;

plan 7;

# L<S04/"The Relationship of Blocks and Declarations" /There is also a let function/>
# L<S04/"Definition of Success">
# let() should not restore the variable if the block exited successfully
# (returned a true value).
{
  my $a = 42;
  {
    let $a = 23;
    is $a, 23, "let() changed the variable (1)";
    1;
  }
  is $a, 23, "let() should not restore the variable, as our block exited succesfully (1)";
}

# let() should restore the variable if the block failed (returned a false
# value).
{
  my $a = 42;
  {
    let $a = 23;
    is $a, 23, "let() changed the variable (1)";
    0;
  }
  is $a, 42, "let() should restore the variable, as our block failed";
}

# Test that let() restores the variable at scope exit, not at subroutine
# entry.
{
  my $a     = 42;
  my $get_a = { $a };
  {
    let $a = 23;
    is $a,       23, "let() changed the variable (2-1)";
    is $get_a(), 23, "let() changed the variable (2-2)";
    1;
  }
  is $a, 23, "let() should not restore the variable, as our block exited succesfully (2)";
}
