#!/usr/bin/pugs

use v6;

require Test;
plan 10;

=head1 DESCRIPTION

Basic C<kv> tests, see S29.

=cut

# L<S29/"Perl6::Arrays" /"kv"/>
{
  my @array = <a b c d>;
  my @kv;
  ok @kv = @array.kv, "basic kv on arrays";
  is +@kv,       8,   "kv on arrays returns the correct number of elems";
  is ~@kv, "0 a 1 b 2 c 3 d",  "array kv has no inner list";
}


# L<S29/"Perl6::Hashes" /"kv"/>
{
  my %hash = (a => 1, b => 2, c => 3, d => 4);
  my @kv;
  ok @kv = %hash.kv.sort, "basic sorted kv on hashes";
  is +@kv,       8,       "kv on hashes returns the correct number of elems";
  is ~@kv, "1 2 3 4 a b c d",  "hash kv has no inner list";
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
