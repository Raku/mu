#!/usr/bin/pugs

require Test;
use v6;

plan 2;

=head1 DESCRIPTION

This test tests the C<reduce> builtin.

Reference:
L<http://groups.google.com/groups?selm=420DB295.3000902%40conway.org>

=cut

my @array = <5 -3 7 0 1 -9>;
my $sum   = 5 + -3 + 7 + 0 + 1 + -9; # laziness :)

eval_is 'reduce:{ $^a + $^b } 0, @array', $sum,
  "basic reduce works (1)", :todo(1);
eval_is 'reduce:{ $^a + $^b } 100, @array', 100 + $sum,
  "basic reduce works (2)", :todo(1);
