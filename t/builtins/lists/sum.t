#!/usr/bin/pugs

use Test;
use v6;

plan 6;

=head1 DESCRIPTION

This test tests the C<sum> builtin.

Reference:
L<"http://groups.google.com/groups?selm=420DB295.3000902%40conway.org">

=cut

my @array = <5 -3 7 0 1 -9>;
my $sum   = 5 + -3 + 7 + 0 + 1 + -9; # laziness :)

is @array.sum,  $sum, "method form of sum on an array works";
is sum(@array), $sum, "subroutine form of sum on an array works";

is sum(-1,2,3), 4,    "subroutine form of sum on a list works";

ok(!defined(sum()), 'sum()ing nothing returns undef');

dies_ok { "a string can't be .summed".sum },
  "method form of sum on a string should not work", :todo<bug>;
dies_ok { 42.sum },
  "method form of sum on a number should not work", :todo<bug>;
