#!/usr/bin/pugs

use v6;
use Test;

plan 16;

is ~(3..6), "3 4 5 6", "(..) works on numbers (1)";
is ~(3..3), "3",       "(..) works on numbers (2)";
is ~(3..2), "",        "(..) works on numbers (3)";

is ~("a".."c"), "a b c", "(..) works on chars (1)";
is ~("a".."a"), "a",     "(..) works on chars (2)";
is ~("b".."a"), "",      "(..) works on chars (3)";

{
  skip 2, "Skipping hanging tests";
  # my @array = 3...;
  # is @array[0], 3, "(...) works (1)";
  # is @array[3], 6, "(...) works (2)";
}

is ~(3..9-3), "3 4 5 6", "(..) has correct precedence (1)";
is ~(2+1..6), "3 4 5 6", "(..) has correct precedence (2)";

# Test the three exclusive range operators:
# L<S03/"New Operators"/binary range operator has variants/>
is [1^..9], [2..9],  "bottom-exclusive range (^..) works";
is [1 ..^9], [1..8], "top-exclusive range (..^) works";
is [1^..^9], [2..8], "double-exclusive range (^..^) works";

is ["a"^.."z"], ["b".."z"], "bottom-exclusive string range (^..) works";
is ["a"..^"z"], ["a".."y"], "top-exclusive string range (..^) works";
is ["a"^..^"z"], ["b".."y"], "double-exclusive string range (^..^) works";

=begin more-discussion-needed

is ~(3..1),     "3 2 1", "(..) works on numbers, left > right";
is ~("c".."a"), "c b a", "(..) works on chars, left > right";
