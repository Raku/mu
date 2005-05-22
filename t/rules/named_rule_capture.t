#!/usr/bin/pugs

use v6;
use Test;

plan 4;

=pod

Testing named capture variables nested inside each other. This doesn't appear to be tested by the ported Perl6::Rules tests. That may be because it's not specified in the synopsis, but Autrijus is sure this how it ought to work.

L<S05/"Rule-scoped variables">

=cut

# At the time of writing, these fail under Win32 so they are marked as bugs
# I haven't yet run them under UNIX but I believe they will work

{
  rule fishy { (.*)shark };
  "whaleshark" ~~ m/<fishy>/;
  eval_is('$/<fishy>[0]', "whale", "named rule ordinal capture", :todo<bug>);
  eval_is('$<fishy>[0]', "whale", "named rule ordinal capture with abbreviated variable", :todo<bug>);
};

{
  my $not_really_a_mammal;
  rule fishy2 { $not_really_a_mammal := (.*)shark };
  "whaleshark" ~~ m/<fishy2>/;
  eval_is('$/<fishy2><not_really_a_mammal>', "whale", "named rule named capture", :todo<bug>);
  eval_is('$<fishy2><not_really_a_mammal>', "whale", "named rule named capture with abbreviated variable", :todo<bug>);
};

