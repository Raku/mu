#!/usr/bin/pugs

use v6;
use Test;

plan 32;

=pod

Tests for the hex() built-in

=cut

# 0 - 9 is the same int
is(hex(0), 0, 'got the correct int value from hex 0');
is(hex(1), 1, 'got the correct int value from hex 1');
is(hex(2), 2, 'got the correct int value from hex 2');
is(hex(3), 3, 'got the correct int value from hex 3');
is(hex(4), 4, 'got the correct int value from hex 4');
is(hex(5), 5, 'got the correct int value from hex 5');
is(hex(6), 6, 'got the correct int value from hex 6');
is(hex(7), 7, 'got the correct int value from hex 7');
is(hex(8), 8, 'got the correct int value from hex 8');
is(hex(9), 9, 'got the correct int value from hex 9');

# check uppercase vals
is(hex("A"), 10, 'got the correct int value from hex A');
is(hex("B"), 11, 'got the correct int value from hex B');
is(hex("C"), 12, 'got the correct int value from hex C');
is(hex("D"), 13, 'got the correct int value from hex D');
is(hex("E"), 14, 'got the correct int value from hex E');
is(hex("F"), 15, 'got the correct int value from hex F');

# check lowercase vals
is(hex("a"), 10, 'got the correct int value from hex a');
is(hex("b"), 11, 'got the correct int value from hex b');
is(hex("c"), 12, 'got the correct int value from hex c');
is(hex("d"), 13, 'got the correct int value from hex d');
is(hex("e"), 14, 'got the correct int value from hex e');
is(hex("f"), 15, 'got the correct int value from hex f');

# check 2 digit numbers
is(hex(10), 16, 'got the correct int value from hex 10');
is(hex(20), 32, 'got the correct int value from hex 20');
is(hex(30), 48, 'got the correct int value from hex 30');
is(hex(40), 64, 'got the correct int value from hex 40');
is(hex(50), 80, 'got the correct int value from hex 50');

# check 2 digit numbers
is(hex(100), 256, 'got the correct int value from hex 100');

# check some weird versions
is(hex("FF"), 255, 'got the correct int value from hex FF');
is(hex("fF"), 255, 'got the correct int value from (mixed case) hex fF');

# some random mad up hex strings (these values are checked against perl5)
is(hex("FFACD5FE"), 4289517054, 'got the correct int value from hex FFACD5FE');
is(hex("AAA4872D"), 2862909229, 'got the correct int value from hex AAA4872D');
