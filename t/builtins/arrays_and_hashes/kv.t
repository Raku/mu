#!/usr/bin/pugs

use v6;

require Test;
plan 12;

=head1 DESCRIPTION

Basic C<kv> tests, see S29.

=cut

# L<S29/"Perl6::Arrays" /"kv"/>
{
  my @array = <a b c d>;
  my @kv;
  ok @kv = @array.kv,   "kv on arrays (1)";
  is +@kv, 4,           "kv on arrays (2)";
  is "@kv[0]",   "0 a", "kv on arrays (3)";
  is "@kv[1]",   "1 b", "kv on arrays (4)";
  is "@kv[2]",   "2 c", "kv on arrays (5)";
  is "@kv[3]",   "3 d", "kv on arrays (6)";
}


# L<S29/"Perl6::Hashes" /"kv"/>
{
  my %hash = (a => 1, b => 2, c => 3, d => 4);
  my @kv;
  ok @kv = %hash.kv.sort,  "kv on hashes (1)";
  is +@kv, 4,              "kv on hashes (2)";
  is "@kv[0]",    "a 1",   "kv on hashes (3)";
  is "@kv[1]",    "b 2",   "kv on hashes (4)";
  is "@kv[2]",    "c 3",   "kv on hashes (5)";
  is "@kv[3]",    "d 4",   "kv on hashes (6)";
}
