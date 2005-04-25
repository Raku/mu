#!/usr/bin/pugs

require Test;
use v6;

=head1 DESCRIPTION

This test tests the behaviour of C<%> when used with
a zero modulus.

All uses of a zero modulus should C<die>, and the
C<die> should be non-fatal.

As soon as the tests don't stop Pugs dead in its
tracks, this test should be merged with 
L<../operators/arith.t>

=cut

plan 2;

my $x;
# eval_ok( '# say 3 % 0', 'Modulo zero dies and is catchable');
# eval_ok( '# my $x=0; say 3 % $x', 'Modulo zero dies and is catchable with variables');

dies_ok( { say 3 % 0 }, 'Modulo zero dies and is catchable');
dies_ok( { $x = 0; say 3 % $x; }, 'Modulo zero dies and is catchable with variables');

