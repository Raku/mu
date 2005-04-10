#!/usr/bin/perl6

use v6;

=head1 Converting between characters and numbers.

You want to convert characters to their numeric value or vice-versa

=cut

my $char = 'a';
my $num  = $char.ord;
say $num;
my $char2 = $num.chr;
say $char2;

$char = 'foo';
say $char.ord; # XXX is this correct behavior?
