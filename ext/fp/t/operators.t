#!/usr/bin/pugs

use v6;
use Test;

BEGIN { plan 14 };

# We can't use "use_ok" here, as the overloaded operators won't go into our
# namespace, then (I think.)
use fp;
pass "fp was use()d successfully.";

{
  my $f = -> $x { $x * 2 };
  my $g = -> $x { $x + 4 };

  is ($f o $g)(5), 18, "(o) works";
  is ($f ∘ $g)(5), 18, "(o) works";
}

is ~({ $_ % 2 == 0 } `grep` [1,2,3,4,5]), "2 4",   "(`grep`) works";
is ~({ $_ * 2 }      `map`  [1,2,3]),     "2 4 6", "(`map`) works";

# is (key ⇒ "value").key, "key", "(⇒) works";

ok 3 ≥ 3, "(≥) works";
ok 3 ≤ 3, "(≤) works";
ok eval('3 ≠ 5'), "(≠) works", :todo<bug>;
{ 
  my $a = 42;
  ok $a ≣ $a, "(≣) works";
  ok $a ≡ $a, "(≡) works";
}

is ∑ [1,2,3], 6, "(∑) works";
is ∏ [1,2,3], 6, "(∏) works";
is 3!,        6, "(!) works";
is 6 ÷ 3,     2, "(÷) works";
