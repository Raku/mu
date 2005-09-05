#!/usr/bin/pugs

use v6;
use Test;

plan 31;

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

# temp() shouldn't change the variable containers
{
  my $a     = 42;
  my $get_a = { $a };
  {
    temp $a = 23;
    ok $a =:= $get_a(), "temp() shouldn't change the variable containers";
  }
}

{
  our $pkgvar = 42;
  {
    temp $pkgvar = 'not 42';
    is $pkgvar, 'not 42', "temp() changed the package variable (3-1)";
  }
  is $pkgvar, 42, "temp() restored the package variable (3-2)";
}

# Test that temp() restores variable even when not exited regularly (using a
# (possibly implicit) call to return()), but when left because of an exception.
{
  my $a = 42;
  try {
    temp $a = 23;
    is $a, 23, "temp() changed the variable in a try block";
    die 57;
  };
  is $a, 42, "temp() restored the variable, the block was exited using an exception";
}

{
  my @array = (0, 1, 2);
  {
    eval 'temp @array[1] = 42';
    is @array[1], 42, "temp() changed our array element", :todo<feature>;
  }
  is @array[1], 1, "temp() restored our array element";
}

{
  my %hash = (:a(1), :b(2), :c(3));
  {
    eval 'temp %hash<b> = 42';
    is %hash<b>, 42, "temp() changed our hash element", :todo<feature>;
  }
  is %hash<b>, 2, "temp() restored our array element";
}

{
  my $struct = [
    "doesnt_matter",
    {
      doesnt_matter => "doesnt_matter",
      key           => [
        "doesnt_matter",
        42,
      ],
    },
  ];

  {
    eval 'temp $struct[1]<key>[1] = 23';
    is $struct[1]<key>[1], 23, "temp() changed our nested arrayref/hashref element", :todo<feature>;
  }
  is $struct[1]<key>[1], 1, "temp() restored our nested arrayref/hashref element";
}

# Block TEMP{}
# L<S06/"Temporization" /You can also modify the behaviour of temporized code structures/>
# (Test is more or less directly from S06.)
{
  my $next    = 0;
  # We stub &advance so we don't need to eval() the whole test.
  sub advance() {}

  # Here is the real implementation of &advance.
  eval 'sub advance() {
    my $curr = $next++;
    TEMP {{ $next = $curr }}  # TEMP block returns the closure { $next = $curr }
    return $curr;
  }';

  # and later...

  is advance(), 0, "TEMP{} block (1)", :todo<feature>;
  is advance(), 1, "TEMP{} block (2)", :todo<feature>;
  is advance(), 2, "TEMP{} block (3)", :todo<feature>;
  is $next,     3, "TEMP{} block (4)", :todo<feature>;

  fail "TEMP{} block (5)", :todo<feature>;
  fail "TEMP{} block (6)", :todo<feature>;
  fail "TEMP{} block (7)", :todo<feature>;
  fail "TEMP{} block (8)", :todo<feature>;

  # Following does parse, but isn't executed (don't know why).
  # If the "{" on the following line is changed to "if(1) {", it is executed,
  # too, but then it dies complaining about not finding a matching temp()
  # function.  So, for now, we just comment the following block and add
  # unconditional fail()s.
  #{
  #  is temp(advance()), 3, "TEMP{} block (5)", :todo<feature>;
  #  is $next,           4, "TEMP{} block (6)", :todo<feature>;
  #  is temp(advance()), 4, "TEMP{} block (7)", :todo<feature>;
  #  is temp(advance()), 5, "TEMP{} block (8)", :todo<feature>;
  #}  # $next = 3

  is $next,     3, "TEMP{} block (9)",  :todo<feature>;
  is advance(), 3, "TEMP{} block (10)", :todo<feature>;
  is $next,     4, "TEMP{} block (11)", :todo<feature>;
}

# Following are OO tests, but I think they fit better in var/temp.t than in
# oo/.
# L<S06/"Temporization" /temp invokes its argument's .TEMP method./>
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
