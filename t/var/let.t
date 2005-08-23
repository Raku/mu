#!/usr/bin/pugs

use v6;
use Test;

plan 9;

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
  is $a, 42, "let() should restore the variable, as our block failed", :todo<feature>;
}

# Test that let() restores the variable at scope exit, not at subroutine
# entry.  (This might be a possibly bug.)
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

# Test that let() restores variable even when not exited regularly (using a
# (possibly implicit) call to return()), but when left because of an exception.
{
  my $a = 42;
  try {
    let $a = 23;
    is $a, 23, "let() changed the variable in a try block";
    die 57;
  };
  is $a, 42, "let() restored the variable, the block was exited using an exception";
}

=pod

Should these work? (They don't even parse currently.)

{
  my @array = (0, 1, 2);
  {
    let @array[1] = 42;
    is @array[1], 42, "let() changed our array element";
    0;
  }
  is @array[1], 1, "let() restored our array element";
}

=cut
