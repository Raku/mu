#!/usr/bin/pugs

use v6;
use Test;

# L<S02/Literals /As with Perl 5 array interpolation/>

plan 5;

{
  my @array = <a b c d>;

  is ~@array, "a b c d",
    "arrays whose elements don't contain whitespace stringify correctly";
}

{
  my @array = <a b c d>;
  push @array, [<e f g h>];

  is ~@array, "a b c d e f g h",
    "arrays with embedded array references stringify correctly";
}

# As with Perl 5 array interpolation, the elements are separated by a space.
# (Except that a space is not added if the element already ends in some kind of
# whitespace. In particular, a list of pairs will interpolate with a tab
# between the key and value, and a newline after the pair.)
{
  my @array = ("a", "b ", "c");

  is ~@array, "a b c",
    "array whose elements do contain whitespace stringify correctly (1)";
}

{
  my @array = ("a\t", "b ", "c");

  is ~@array, "a\tb c",
    "array whose elements do contain whitespace stringify correctly (2)";
}

{
  my @array = ("a\t", " b ", "c");

  is ~@array, "a\t b c",
    "array whose elements do contain whitespace stringify correctly (3)";
}
