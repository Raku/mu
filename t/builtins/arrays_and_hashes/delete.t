#!/usr/bin/pugs

use v6;

require Test;
plan 11;

=head1 DESCRIPTION

Basic C<delete> tests, see S29.

=cut

# L<S29/"Perl6::Arrays" /"delete"/>
my @array = <a b c d>;
is ~@array, "a b c d",                 "basic sanity (1)";
is ~@array.delete(2), "c",
  "deletion of an array element returned the right thing";
# Note: The double space here is correct (it's the stringification of undef).
is ~@array, "a b  d",              "deletion of an array element";
is ~@array.delete(0, 3), "a d",
  "deletion of array elements returned the right things";
is ~@array, " b ",                  "deletion of array elements";

# L<S29/"Perl6::Hashes" /"delete"/>
my %hash = (a => 1, b => 2, c => 3, d => 4);
is +%hash, 4, "basic sanity (2)";
is ~%hash.delete("a"), "1",
  "deletion of a hash element returned the right value";
is +%hash, 3, "deletion of a hash element";
is ~%hash.delete("c", "d"), "3 4",
  "deletion of hash elements returned the right values";
is +%hash, 1, "deletion of hash elements";
ok !defined(%hash{"a"}), "deleted hash elements are really deleted";
