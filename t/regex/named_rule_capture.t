use v6-alpha;

use Test;

plan 4;

=pod

Testing named capture variables nested inside each other. This doesn't appear to be tested by the ported Perl6::Rules tests. That may be because it's not specified in the synopsis, but Autrijus is sure this how it ought to work.

=cut

# At the time of writing, these fail under Win32 so they are marked as bugs
# I haven't yet run them under UNIX but I believe they will work

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
  exit;
}

#L<S05/Nested subpattern captures>

{
  regex fishy { (.*)shark };
  "whaleshark" ~~ m/<fishy>/;
  is(eval('$/<fishy>[0]'), "whale", "named rule ordinal capture");
  is(eval('$<fishy>[0]'), "whale", "named rule ordinal capture with abbreviated variable");
};

#L<S05/Named scalar aliasing to subpatterns>

{
  my $not_really_a_mammal;
  regex fishy2 { $not_really_a_mammal := (.*)shark };
  "whaleshark" ~~ m/<fishy2>/;
  is(eval('$/<fishy2><not_really_a_mammal>'), "whale", "named rule named capture", :todo<bug>);
  is(eval('$<fishy2><not_really_a_mammal>'), "whale", "named rule named capture with abbreviated variable", :todo<bug>);
};

