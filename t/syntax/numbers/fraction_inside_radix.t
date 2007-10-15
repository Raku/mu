use v6-alpha;

use Test;

plan 1;

=pod

Tests for the :x[ <list> ] notations

=cut

# L<S02/Literals/"Any radix may include a fractional part">

is( :16<dead_beef.face>,  0xDEAD_BEEF + 0xFACE / ( 16 ** 4 ), 
	'Fractional base 16 works' );

