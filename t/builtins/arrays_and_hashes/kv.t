#!/usr/bin/pugs

use v6;

require Test;
plan 20;

=head1 DESCRIPTION

Basic C<kv> tests, see S29.

=cut

# L<S29/"Perl6::Arrays" /"kv"/>
{
  my @array = <a b c d>;
  my @kv;
  ok @kv = @array.kv,   "kv on arrays (1)";
  is +@kv, 4,           "kv on arrays (2)";
  is @kv[0].key,   0,   "kv on arrays (3)";
  is @kv[1].key,   1,   "kv on arrays (4)";
  is @kv[2].key,   2,   "kv on arrays (5)";
  is @kv[3].key,   3,   "kv on arrays (6)";
  is @kv[0].value, "a", "kv on arrays (7)";
  is @kv[1].value, "b", "kv on arrays (8)";
  is @kv[2].value, "c", "kv on arrays (9)";
  is @kv[3].value, "d", "kv on arrays (10)";
}


# L<S29/"Perl6::Hashes" /"kv"/>
{
  my %hash = (a => 1, b => 2, c => 3, d => 4);
  my @kv;
  ok @kv = %hash.kv.sort,   "kv on hashes (1)";
  is +@kv, 4,               "kv on hashes (2)";
  is @kv[0].value,  1,      "kv on hashes (3)";
  is @kv[1].value,  2,      "kv on hashes (4)";
  is @kv[2].value,  3,      "kv on hashes (5)";
  is @kv[3].value,  4,      "kv on hashes (6)";
  is @kv[0].key,    "a",    "kv on hashes (7)";
  is @kv[1].key,    "b",    "kv on hashes (8)";
  is @kv[2].key,    "c",    "kv on hashes (9)";
  is @kv[3].key,    "d",    "kv on hashes (10)";
}
