#!/usr/bin/pugs

use v6;
use Test;

plan 9;

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

# Following are OO tests, but I think they fit better in var/temp.t than in
# oo/.
# L<S06/"Temporization">
{
  my $was_in_own_temp_handler = 0;
  eval '
    class WierdTemp is Int {
      method TEMP {
	$was_in_own_temp_handler++;
	return { $was_in_own_temp_handler++ };
      }
  ';

  my $a = eval 'WierdTemp.new()';
  ok defined($a), "instantiating a WierdTemp worked", :todo<feature>;
  is $was_in_own_temp_handler, 0, ".TEMP method wasn't yet executed";

  {
    temp $a;
    is $was_in_own_temp_handler, 1, ".TEMP method was executed on temporization", :todo<feature>;
  }
  is $was_in_own_temp_handler, 2, ".TEMP method was executed on restoration", :todo<feature>;
}
