#!/usr/bin/pugs

require Test;
use v6;

plan 2;

=head1 DESCRIPTION

This test tests the C<sum> builtin.

Reference:
L<http://groups.google.com/groups?selm=420DB295.3000902%40conway.org>

=cut

my @array = <5 -3 7 0 1 -9>;
my $sum   = 5 + -3 + 7 + 0 + 1 + -9; # laziness :)

todo_eval_is '@array.sum', $sum, "method form of sum works";
todo_eval_is 'sum @array', $sum, "subroutine form of sum works";
