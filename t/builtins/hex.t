#!/usr/bin/pugs

use v6;
require Test;

plan 32;

=pod

Tests for the hex() built-in

=cut

# 0 - 9 is the same int
todo_eval_is('hex(0)', 0, 'got the correct int value from hex 0');
todo_eval_is('hex(1)', 1, 'got the correct int value from hex 1');
todo_eval_is('hex(2)', 2, 'got the correct int value from hex 2');
todo_eval_is('hex(3)', 3, 'got the correct int value from hex 3');
todo_eval_is('hex(4)', 4, 'got the correct int value from hex 4');
todo_eval_is('hex(5)', 5, 'got the correct int value from hex 5');
todo_eval_is('hex(6)', 6, 'got the correct int value from hex 6');
todo_eval_is('hex(7)', 7, 'got the correct int value from hex 7');
todo_eval_is('hex(8)', 8, 'got the correct int value from hex 8');
todo_eval_is('hex(9)', 9, 'got the correct int value from hex 9');

# check uppercase vals
todo_eval_is('hex("A")', 10, 'got the correct int value from hex A');
todo_eval_is('hex("B")', 11, 'got the correct int value from hex B');
todo_eval_is('hex("C")', 12, 'got the correct int value from hex C');
todo_eval_is('hex("D")', 13, 'got the correct int value from hex D');
todo_eval_is('hex("E")', 14, 'got the correct int value from hex E');
todo_eval_is('hex("F")', 15, 'got the correct int value from hex F');

# check lowercase vals
todo_eval_is('hex("a")', 10, 'got the correct int value from hex a');
todo_eval_is('hex("b")', 11, 'got the correct int value from hex b');
todo_eval_is('hex("c")', 12, 'got the correct int value from hex c');
todo_eval_is('hex("d")', 13, 'got the correct int value from hex d');
todo_eval_is('hex("e")', 14, 'got the correct int value from hex e');
todo_eval_is('hex("f")', 15, 'got the correct int value from hex f');

# check 2 digit numbers
todo_eval_is('hex(10)', 16, 'got the correct int value from hex 10');
todo_eval_is('hex(20)', 32, 'got the correct int value from hex 20');
todo_eval_is('hex(30)', 48, 'got the correct int value from hex 30');
todo_eval_is('hex(40)', 64, 'got the correct int value from hex 40');
todo_eval_is('hex(50)', 80, 'got the correct int value from hex 50');

# check 2 digit numbers
todo_eval_is('hex(100)', 256, 'got the correct int value from hex 100');

# check some weird versions
todo_eval_is('hex("FF")', 255, 'got the correct int value from hex FF');
todo_eval_is('hex("fF")', 255, 'got the correct int value from (mixed case) hex fF');

# some random mad up hex strings (these values are checked against perl5)
todo_eval_is('hex("FFACD5FE")', 4289517054, 'got the correct int value from hex FFACD5FE');
todo_eval_is('hex("AAA4872D")', 2862909229, 'got the correct int value from hex AAA4872D');
