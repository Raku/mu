use v6-alpha;

use Test;

plan 38;

=pod

Tests for the :16() built-in

=cut

# L<S29/Conversions/"prefix:<:16>">
# L<S02/Literals/":16<DEAD_BEEF>">

# 0 - 9 is the same int
is(:16(0), 0, 'got the correct int value from hex 0');
is(:16(1), 1, 'got the correct int value from hex 1');
is(:16(2), 2, 'got the correct int value from hex 2');
is(:16(3), 3, 'got the correct int value from hex 3');
is(:16(4), 4, 'got the correct int value from hex 4');
is(:16(5), 5, 'got the correct int value from hex 5');
is(:16(6), 6, 'got the correct int value from hex 6');
is(:16(7), 7, 'got the correct int value from hex 7');
is(:16(8), 8, 'got the correct int value from hex 8');
is(:16(9), 9, 'got the correct int value from hex 9');

# check uppercase vals
is(:16("A"), 10, 'got the correct int value from hex A');
is(:16("B"), 11, 'got the correct int value from hex B');
is(:16("C"), 12, 'got the correct int value from hex C');
is(:16("D"), 13, 'got the correct int value from hex D');
is(:16("E"), 14, 'got the correct int value from hex E');
is(:16("F"), 15, 'got the correct int value from hex F');

# check lowercase vals
is(:16("a"), 10, 'got the correct int value from hex a');
is(:16("b"), 11, 'got the correct int value from hex b');
is(:16("c"), 12, 'got the correct int value from hex c');
is(:16("d"), 13, 'got the correct int value from hex d');
is(:16("e"), 14, 'got the correct int value from hex e');
is(:16("f"), 15, 'got the correct int value from hex f');

# check 2 digit numbers
is(:16(10), 16, 'got the correct int value from hex 10');
is(:16(20), 32, 'got the correct int value from hex 20');
is(:16(30), 48, 'got the correct int value from hex 30');
is(:16(40), 64, 'got the correct int value from hex 40');
is(:16(50), 80, 'got the correct int value from hex 50');

# check 3 digit numbers
is(:16(100), 256, 'got the correct int value from hex 100');

# check some weird versions
is(:16("FF"), 255, 'got the correct int value from hex FF');
is(:16("fF"), 255, 'got the correct int value from (mixed case) hex fF');

# some random mad up hex strings (these values are checked against perl5)
is :16("FFACD5FE"), 4289517054, 'got the correct int value from hex FFACD5FE';
is :16("AAA4872D"), 2862909229, 'got the correct int value from hex AAA4872D';
is :16<DEAD_BEEF>,  0xDEADBEEF, 'got the correct int value from hex DEAD_BEEF';

# L<S02/Literals/"interpret leading 0b or 0d as hex digits">
is(:16<0b1110>, 0xB1110, ':16<0b1110> uses b as hex digit'  );
is(:16<0d37>,   0x0D37,  ':16<0d37> uses d as hex digit'     );

# L<S02/Literals/"Think of these as setting the default radix">
is(:16<0x20>,      0d32, ':16<0x20> stays hexadecimal',        :todo<feature> );
is(:16<0o377>,    0d255, ':16<0o255> converts from octal',     :todo<feature> );

# L<S02/Literals/"which will be interpreted as they would outside the string">
# It seems odd that the numbers on the inside on the <> would be a mix of
# bases. Maybe I've misread the paragraph -- brian
is( :16<dead_beef> * 16**8, :16<dead_beef*16**8>, 
	'Powers outside same as powers inside', :todo<feature> );

