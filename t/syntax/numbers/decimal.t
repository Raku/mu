use v6-alpha;

use Test;

plan 11;

=pod

Tests for the :2() built-in

=cut

# L<S02/Literals/":10<42>">

is( :10(0),   0, 'got the correct int value from decimal 0' );
is( :10(1),   1, 'got the correct int value from decimal 1' );
is( :10(2), 0d2, 'got the correct int value from decimal 2' );
is( :10(3), 0d3, 'got the correct int value from decimal 3' );

# the answer to everything
is(     42,   0d42, '42 and 0d42 are the same'      );
is( :10<42>,    42, ':10<42> and 42 are the same'   );
is( :10<42>,  0d42, ':10<42> and 0d42 are the same' );

# L<S02/Literals/"Think of these as setting the default radix">
# setting the default radix

is( :10<0b1110>,  0d14, ':10<0b1110> converts from binary',    :todo<feature> );
is( :10<0x20>,    0d32, ':10<0x20> converts from hexadecimal', :todo<feature> );
is( :10<0o377>,  0d255, ':10<0o255> converts from octal',      :todo<feature> );
is( :10<0d37>,    0d37, ':10<0d37> stays from decimal',        :todo<feature> );
