#!/usr/bin/pugs

use v6;
use Test;

# See http://use.perl.org/~autrijus/journal/25337 and
# http://www.nntp.perl.org/group/perl.perl6.language/22532.

plan 18;

{
  my $x = 3;
  my $y = \$x;

  ok !$y.isa("Int"), "refs to 'non-objects' should not autoderef", :todo<bug>;
  dies_ok { +$y },   "'real refs' don't numify", :todo<bug>;
  dies_ok { ~$y },   "'real refs' don't stringify", :todo<bug>;

  $x = undef;
  ok ?$y, "'real refs' always booleanify to true";
}

{
  my @x = (1,2,3);
  my $y = \@x;

  ok $y.isa("Array"), "refs to non-'non-objects' should autoderef";
  is +$y, 3,          "refs to non-'non-objects' should numify";
  is ~$y, "1 2 3",    "refs to non-'non-objects' should stringify";
  ok ?$y,             "refs to non-'non-objects' should booleanify (1)";

  @x = ();
  ok !?$y,            "refs to non-'non-objects' should booleanify (2)";
}

{
  my %x = (:a(1), :b(2), :c(3));
  my $y = \%x;

  ok $y.isa("Hash"), "refs to non-'non-objects' should autoderef";
  is +$y, 3,         "refs to non-'non-objects' should numify";
  ok ~$y,            "refs to non-'non-objects' should stringify";
  ok ?$y,            "refs to non-'non-objects' should booleanify (1)";

  %x = ();
  ok !?$y,           "refs to non-'non-objects' should booleanify (2)";
}

{
  my @x = (1,2,3);
  my $y = \@x;
  my $z = \$y;

  ok !$z.isa("Array"), "refs to Refs should not autoderef", :todo<bug>;
  dies_ok { +$z },     "refs to Refs don't numify", :todo<bug>;
  dies_ok { ~$z },     "refs to Refs don't stringify", :todo<bug>;

  @x = ();
  ok ?$z, "refs to Refs always booleanify to true", :todo<bug>;
}

=begin more-discussion-needed

# Tests for &tied.
{
  my @x = (1,2,3);
  my $y = \@x;

  ok !tied($y).isa("Array"), "tied with 'fake refs' (1)";
  ok  tied($y).isa("Ref"),   "tied with 'fake refs' (2)";
  dies_ok { +tied($y) },     "tied with 'fake refs' (3)";
  dies_ok { ~tied($y) },     "tied with 'fake refs' (4)";
  ok ?tied($y),              "tied with 'fake refs' (5)";

  @x = ();
  ok ?tied($y),              "tied with 'fake refs' (6)";
}

{
  my $x = 3;
  my $y = \$x;

  ok !tied($y).isa("Array"), "tied with 'real refs' (1)";
  ok  tied($y).isa("Ref"),   "tied with 'real refs' (2)";
  dies_ok { +tied($y) },     "tied with 'real refs' (3)";
  dies_ok { ~tied($y) },     "tied with 'real refs' (4)";
  ok ?tied($y),              "tied with 'real refs' (5)";

  $x = 0;
  ok ?tied($y),              "tied with 'real refs' (6)";
}
