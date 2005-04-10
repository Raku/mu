#!/usr/bin/perl6

use v6;

=head1 Swapping values

You want to swap values without using a temporary variable

=cut

my ($x, $y) = (3,2);
($x, $y) = ($y, $x);
# XXX Binding (:=) is more efficient, because it doesn't copy the values.
# XXX I don't know if compile time binding (::=) would be even better here.
say $x;
say $y;
