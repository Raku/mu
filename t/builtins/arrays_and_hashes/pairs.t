#!/usr/bin/pugs

use v6;

require Test;
plan 20;

=head1 DESCRIPTION

Basic C<pairs> tests, see S29.

=cut

# L<S29/"Perl6::Arrays" /"pairs"/>
{
  my @array = <a b c d>;
  my @pairs;
  ok @pairs = @array.pairs,   "pairs on arrays (1)";
  is +@pairs, 4,           "pairs on arrays (2)";
  is @pairs[0].key,   0,   "pairs on arrays (3)";
  is @pairs[1].key,   1,   "pairs on arrays (4)";
  is @pairs[2].key,   2,   "pairs on arrays (5)";
  is @pairs[3].key,   3,   "pairs on arrays (6)";
  is @pairs[0].value, "a", "pairs on arrays (7)";
  is @pairs[1].value, "b", "pairs on arrays (8)";
  is @pairs[2].value, "c", "pairs on arrays (9)";
  is @pairs[3].value, "d", "pairs on arrays (10)";
}


# L<S29/"Perl6::Hashes" /"pairs"/>
{
  my %hash = (a => 1, b => 2, c => 3, d => 4);
  my @pairs;
  ok @pairs = %hash.pairs.sort,"pairs on hashes (1)";
  is +@pairs, 4,               "pairs on hashes (2)";
  is @pairs[0].value,  1,      "pairs on hashes (3)";
  is @pairs[1].value,  2,      "pairs on hashes (4)";
  is @pairs[2].value,  3,      "pairs on hashes (5)";
  is @pairs[3].value,  4,      "pairs on hashes (6)";
  is @pairs[0].key,    "a",    "pairs on hashes (7)";
  is @pairs[1].key,    "b",    "pairs on hashes (8)";
  is @pairs[2].key,    "c",    "pairs on hashes (9)";
  is @pairs[3].key,    "d",    "pairs on hashes (10)";
}
