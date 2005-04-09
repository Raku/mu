#!perl6

use v6;

=head1 Default values

You want to provide default values for false or undefined variables.

=cut

my $x = 0;

# XXX - mention // first? I don't even think we should associate || with
# defaults anymore

# to provide a default for false values
$x ||= 2;
say $x;

# to provide a default for undefined values
my $y;
$y //= 3;
say $y;

#my $z is "Ovid" but false;
#$z ||= 4;
#say $z;
