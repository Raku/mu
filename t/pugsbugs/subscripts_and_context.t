#!/usr/bin/pugs

use v6;
require Test;

plan 2;

# "The context inside of hash and array scripts seems to be/is wrong"

{
  sub return_01 { my @sub_array = ("0", "1"); return @sub_array }

  my @array  = <a b c d>;
  my @sliced = @array[return_01()];
  # @sliced *should* be <a b>, but it is <c>.
  # This is because return_012() is called in numeric context, and so return_012
  # returns the *number* of elems in @sub_array instead of the array @sub_array.
  is ~@sliced, "a b", "context inside of array subscripts";
}

# Same for hashes.
{
  sub return_ab { my @sub_array = <a b>; return @sub_array }

  my %hash   = (a => 1, b => 2, c => 3);
  my @sliced = %hash{return_ab()};
  # @sliced *should* be ("1, "2").
  # The above for bug explanation.
  is ~@sliced, "1 2", "context inside of hash subscripts";
}
