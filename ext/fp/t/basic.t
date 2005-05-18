#!/usr/bin/pugs

use v6;
use Test;

BEGIN { plan 4 };

# We can't use "use_ok" here, as the overloaded operators won't go into our
# namespace, then (I think.)
use fp;
pass "fp was use()d successfully.";

{
  my $f = -> $x { $x * 2 };
  my $g = -> $x { $x + 4 };

  is ($f o $g)(5), 18, "(o) works";
}

is ~({ $_ % 2 == 0 } `grep` [1,2,3,4,5]), "2 4",   "(`grep`) works";
is ~({ $_ * 2 }      `map`  [1,2,3]),     "2 4 6", "(`map`) works";
