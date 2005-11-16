#!/usr/bin/pugs

use v6;
use Test;

plan 20;

# 3..2 must *not* produce "3 2".  Use reverse to get a reversed range. -lwall

is ~(3..6), "3 4 5 6", "(..) works on numbers (1)";
is ~(3..3), "3",       "(..) works on numbers (2)";
is ~(3..2), "",        "(..) works on numbers (3)";
is ~(8..11), "8 9 10 11",   "(..) works on carried numbers (3)";

is ~("a".."c"), "a b c", "(..) works on chars (1)";
is ~("a".."a"), "a",     "(..) works on chars (2)";
is ~("b".."a"), "",      "(..) works on chars (3)";
is ~("Y".."AB"), "Y Z AA AB", "(..) works on carried chars (3)";

{
  skip 2, "Skipping hanging tests";
  # my @array = 3...;
  # is @array[0], 3, "(...) works (1)";
  # is @array[3], 6, "(...) works (2)";
}

is ~(3..9-3), "3 4 5 6", "(..) has correct precedence (1)";
is ~(2+1..6), "3 4 5 6", "(..) has correct precedence (2)";

# Test the three exclusive range operators:
# L<S03/"New Operators" /binary range operator has variants/>
is [1^..9], [2..9],  "bottom-exclusive range (^..) works";
is [1 ..^9], [1..8], "top-exclusive range (..^) works";
is [1^..^9], [2..8], "double-exclusive range (^..^) works";
is [1^..^2], [], "double-exclusive range (^..^) can produce null range";

is ["a"^.."z"], ["b".."z"], "bottom-exclusive string range (^..) works";
is ["a"..^"z"], ["a".."y"], "top-exclusive string range (..^) works";
is ["a"^..^"z"], ["b".."y"], "double-exclusive string range (^..^) works";
is ['a'^..^'b'], [], "double-exclusive string range (^..^) can produce null range";


