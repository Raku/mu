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
  eval_ok '@pairs = @array.kv', "kv on arrays (1)";
  is      +@kv, 4,              "kv on arrays (2)";
  eval_is '@kv[0][0]',   0,     "kv on arrays (3)";
  eval_is '@kv[1][0]',   1,     "kv on arrays (4)";
  eval_is '@kv[2][0]',   2,     "kv on arrays (5)";
  eval_is '@kv[3][0]',   3,     "kv on arrays (6)";
  eval_is '@kv[0][1]', "a",     "kv on arrays (7)";
  eval_is '@kv[1][1]', "b",     "kv on arrays (8)";
  eval_is '@kv[2][1]', "c",     "kv on arrays (9)";
  eval_is '@kv[3][1]', "d",     "kv on arrays (10)";
}


# L<S29/"Perl6::Hashes" /"kv"/>
{
  my %hash = (a => 1, b => 2, c => 3, d => 4);
  my @kv;
  eval_ok '@pairs = %hash.kv', "kv on hashes (1)";
  is      +@kv, 4,             "kv on hashes (2)";
  eval_is '@kv[0][0]',   0,    "kv on hashes (3)";
  eval_is '@kv[1][0]',   1,    "kv on hashes (4)";
  eval_is '@kv[2][0]',   2,    "kv on hashes (5)";
  eval_is '@kv[3][0]',   3,    "kv on hashes (6)";
  eval_is '@kv[0][1]', "a",    "kv on hashes (7)";
  eval_is '@kv[1][1]', "b",    "kv on hashes (8)";
  eval_is '@kv[2][1]', "c",    "kv on hashes (9)";
  eval_is '@kv[3][1]', "d",    "kv on hashes (10)";
}
