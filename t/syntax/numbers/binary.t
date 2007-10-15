use v6-alpha;

use Test;

plan 16;

=pod

Tests for the :2() built-in

=cut

# L<S29/Conversions/"prefix:<:2>">

is(:2(0),     0, 'got the correct int value from bin 0');
is(:2(1),     1, 'got the correct int value from bin 1');
is(:2(10),    2, 'got the correct int value from bin 10');
is(:2(1010), 10, 'got the correct int value from bin 1010');

is(
	:2(11111111111111111111111111111111), 
	0xFFFFFFFF, 
	'got the correct int value from bin 11111111111111111111111111111111');


# L<S02/Literals/"Think of these as setting the default radix">
# setting the default radix

is(:2<0b1110>,  0d14, ':2<0b1110> stays binary',            :todo<feature> );
is(:2<0x20>,    0d32, ':2<0x20> converts from hexadecimal', :todo<feature> );
is(:2<0o377>,  0d255, ':2<0o255> converts from octal',      :todo<feature> );
is(:2<0d37>,    0d37, ':2<0d37> converts from decimal',     :todo<feature> );

# L<S02/Literals/"not clear whether the exponentiator should be 10 or the radix">

isnt( eval("0b1.1e10"), 1536, 'Ambiguous, illegal syntax doesn\'t work' ); # Ambiguous, not allowed

# L<S02/Literals/"and this makes it explicit">
# probably don't need a test, but I'll write tests for any example :)
is( :2<1.1> *  2 ** 10,                  1536, 'binary number to power of 2'  );
is( :2<1.1> * 10 ** 10,        15_000_000_000, 'binary number to power of 10' );
is( :2<1.1> * :2<10> ** :2<10>,             6, 'multiplication and exponentiation' );

# L<S02/Literals/"So we write those as">
# these should be the same values as the previous tests
is( :2<1.1*2**10>,                   1536, 'Power of two in <> works', :todo<feature> );
is( :2<1.1*10**10>,        15_000_000_000, 'Power of ten in <> works', :todo<feature> );
is( eval('2«1.1*:2<10>**:2<10>»'),    6, 'Powers of two in <<>> works', :todo<feature> );