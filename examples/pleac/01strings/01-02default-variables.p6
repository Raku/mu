#!/usr/bin/perl6

use v6;

=head1 Default values

You want to provide default values for false or undefined variables.

=cut

# to provide a default for undefined values
my $y;
$y //= 3;
say $y;

# to provide a default for false values (similar to Perl 5.  Should be used
# with caution)

my $x = 0;
$x ||= 2;
say $x;

#my $z is "Ovid" but false;
#$z ||= 4;
#say $z;
