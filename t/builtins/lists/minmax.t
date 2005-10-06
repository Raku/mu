#!/usr/bin/pugs

use Test;
use v6;

plan 12;

=head1 DESCRIPTION

This test tests the C<min> and C<max> builtins.

Reference:
L<http://groups.google.com/groups?selm=420DB295.3000902%40conway.org>

=cut

my @array = <5 -3 7 0 1 -9>;

# Tests for C<min>:
is @array.min,  -9, "basic method form of min works";
is min(@array), -9, "basic subroutine form of min works";
is @array.min:{ abs $^a <=> abs $^b }, 0,
  "method form of min taking a comparision block works";
is min({ abs $^a <=> abs $^b }, @array), 0,
  "subroutine form of min taking a comparision block works";

# Tests for C<max>:
is @array.max,  7, "basic method form of max works";
is max(@array), 7, "basic subroutine form of max works";
is @array.max:{ abs $^a <=> abs $^b }, -9,
  "method form of max taking a comparision block works";
is max({ abs $^a <=> abs $^b }, @array), -9,
  "subroutine form of max taking a comparision block works";

# Error cases
dies_ok { 42.max }, ".max should not work on scalars", :todo<bug>;
dies_ok { 42.min }, ".min should not work on scalars", :todo<bug>;
is (42,).max, 42, ".max should work on one-elem arrays";
is (42,).max, 42, ".max should work on one-elem arrays";
