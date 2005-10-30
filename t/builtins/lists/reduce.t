#!/usr/bin/pugs

use Test;
use v6;

=head1 DESCRIPTION

This test tests the C<reduce> builtin.

Reference:
L<"http://groups.google.com/groups?selm=420DB295.3000902%40conway.org">

=cut

plan 10;

{
  my @array = <5 -3 7 0 1 -9>;
  my $sum   = 5 + -3 + 7 + 0 + 1 + -9; # laziness :)

  is((reduce { $^a + $^b } 0, @array), $sum, "basic reduce works (1)");
  is((reduce { $^a + $^b } 100, @array), 100 + $sum, "basic reduce works (2)");
}

# Reduce with n-ary functions
{
  my @array  = <1 2 3 4 5 6 7 8>;
  my $result = (((1 + 2 * 3) + 4 * 5) + 6 * 7) + 8 * undef;

  is @array.reduce:{ $^a + $^b * $^c }, $result, "n-ary reduce() works";
}

# .reduce shouldn't work on non-arrays
{
  dies_ok { 42.reduce:{ $^a + $^b } },    "method form of reduce should not work on numbers", :todo<bug>;
  dies_ok { "str".reduce:{ $^a + $^b } }, "method form of reduce should not work on strings", :todo<bug>;
  is (42,).reduce:{ $^a + $^b }, 42,      "method form of reduce should work on arrays";
}

{
  my $hash = {a => {b => {c => 42}}};
  my @reftypes;
  sub foo (Hash $hash, String $key) {
    push @reftypes, $hash.ref;
    $hash.{$key};
  }
  is((reduce(&foo, $hash, <a b c>)), 42, 'reduce(&foo) (foo ~~ .{}) works three levels deep');
  is(@reftypes[0], "Hash", "first application of reduced hash subscript passed in a Hash");
  is(@reftypes[1], "Hash", "second application of reduced hash subscript passed in a Hash");
  is(@reftypes[2], "Hash", "third application of reduced hash subscript passed in a Hash");
}
