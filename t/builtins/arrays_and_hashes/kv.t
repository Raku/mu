#!/usr/bin/pugs

use v6;

require Test;
plan 18;

=head1 DESCRIPTION

Basic C<kv> tests, see S29.

=cut

# L<S29/"Perl6::Arrays" /"kv"/>
{
  my @array = <a b c d>;
  my @kv;
  ok @kv = @array.kv, "basic kv on arrays";
  is +@kv,       4,   "kv on arrays returns the correct number of elems";
  is +@kv[0],    2,   "array kv's inner lists have two elems";
  is ~@kv[0], "0 a",  "array kv's inner list matched expectation (1)";
  is ~@kv[1], "1 b",  "array kv's inner list matched expectation (2)";
  is ~@kv[2], "2 c",  "array kv's inner list matched expectation (3)";
  is ~@kv[3], "3 d",  "array kv's inner list matched expectation (4)";
}


# L<S29/"Perl6::Hashes" /"kv"/>
{
  my %hash = (a => 1, b => 2, c => 3, d => 4);
  my @kv;
  ok @kv = %hash.kv.sort, "basic sorted kv on hashes";
  is +@kv,       4,       "kv on hashes returns the correct number of elems";
  is +@kv[0],    2,       "hash kv's inner lists have two elems";
  is ~@kv[0], "a 1",      "hash kv's inner list matched expectation (1)";
  is ~@kv[1], "b 2",      "hash kv's inner list matched expectation (2)";
  is ~@kv[2], "c 3",      "hash kv's inner list matched expectation (3)";
  is ~@kv[3], "d 4",      "hash kv's inner list matched expectation (4)";
}

# Following stated by Larry on p6l
{
  my $pair  = (a => 1);
  my @kv;
  ok @kv = $pair.kv, "kv on a pair";
  is +@kv,       1,  "kv on a pair returned one elem";
  is +@kv[0],    2,  "pair kv's inner list has two elems";
  is ~@kv[0], "a 1", "pair kv's inner list matched expectation";
}
