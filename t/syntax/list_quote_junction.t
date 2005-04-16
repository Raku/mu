#!/usr/bin/pugs

use v6;
require Test;

=kwid

= DESCRIPTION

Tests that the C<any()> and list quoting constructs
play well together and match well.

The following should match:

  "foo" ~~ any<foo bar baz>
  "foo" ~~ any(<foo bar baz>)
  "bar" ~~ any<foo bar baz>
  "bar" ~~ any(<foo bar baz>)

The following should not match:

  "fo"      ~~ any<foo bar baz>
  "oo"      ~~ any<foo bar baz>
  "bar b"   ~~ any<foo bar baz>
  "bar baz" ~~ any(<foo bar baz>)

I'm told that C<< any<foo bar> >> will become a syntax
error. Until then, I hope it will stay.

=cut

my @matching_strings = <foo bar>;
my @nonmatching_strings = ('fo', 'oo', 'bar b', 'bar baz');

plan ((+@matching_strings+@nonmatching_strings)*2);

for @matching_strings -> $str {
  ok( $str ~~ (any<foo bar baz>), "'$str' matches any<foo bar baz>" );
  ok( $str ~~ any(<foo bar baz>), "'$str' matches any(<foo bar baz>)" );
};

for @nonmatching_strings -> $str {
  ok( ($str !~ any<foo bar baz>), "'$str' does not match any<foo bar baz>" );
  ok( $str !~ any(<foo bar baz>), "'$str' does not match any(<foo bar baz>)" );
};
