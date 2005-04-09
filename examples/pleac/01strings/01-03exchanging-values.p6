#!perl6

use v6;

=head1 Swapping values

You want to swap values without using a temporary variable

=cut

my ($x, $y) = (3,2);
($x, $y) = ($y, $x);
say $x;
say $y;
