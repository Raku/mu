#!/usr/bin/pugs

use v6;
use Test;

plan 95;

=pod

Basic tests for the char() builtin

NOTE: these tests only deal with ASCII

=cut

# What is the best way to test 0 through 31??

is(chr(32), ' ', 'got the right char for 32');
is(chr(33), '!', 'got the right char for 33');
is(chr(34), '"', 'got the right char for 34'); #"
is(chr(35), '#', 'got the right char for 35');
is(chr(36), '$', 'got the right char for 36');
is(chr(37), '%', 'got the right char for 37');
is(chr(38), '&', 'got the right char for 38');
is(chr(39), "'", 'got the right char for 39');
is(chr(40), '(', 'got the right char for 40');
is(chr(41), ')', 'got the right char for 41');
is(chr(42), '*', 'got the right char for 42');
is(chr(43), '+', 'got the right char for 43');
is(chr(44), ',', 'got the right char for 44');
is(chr(45), '-', 'got the right char for 45');
is(chr(46), '.', 'got the right char for 46');
is(chr(47), '/', 'got the right char for 47');
is(chr(48), '0', 'got the right char for 48');
is(chr(49), '1', 'got the right char for 49');
is(chr(50), '2', 'got the right char for 50');
is(chr(51), '3', 'got the right char for 51');
is(chr(52), '4', 'got the right char for 52');
is(chr(53), '5', 'got the right char for 53');
is(chr(54), '6', 'got the right char for 54');
is(chr(55), '7', 'got the right char for 55');
is(chr(56), '8', 'got the right char for 56');
is(chr(57), '9', 'got the right char for 57');
is(chr(58), ':', 'got the right char for 58');
is(chr(59), ';', 'got the right char for 59');
is(chr(60), '<', 'got the right char for 60');
is(chr(61), '=', 'got the right char for 61');
is(chr(62), '>', 'got the right char for 62');
is(chr(63), '?', 'got the right char for 63');
is(chr(64), '@', 'got the right char for 64');
is(chr(65), 'A', 'got the right char for 65');
is(chr(66), 'B', 'got the right char for 66');
is(chr(67), 'C', 'got the right char for 67');
is(chr(68), 'D', 'got the right char for 68');
is(chr(69), 'E', 'got the right char for 69');
is(chr(70), 'F', 'got the right char for 70');
is(chr(71), 'G', 'got the right char for 71');
is(chr(72), 'H', 'got the right char for 72');
is(chr(73), 'I', 'got the right char for 73');
is(chr(74), 'J', 'got the right char for 74');
is(chr(75), 'K', 'got the right char for 75');
is(chr(76), 'L', 'got the right char for 76');
is(chr(77), 'M', 'got the right char for 77');
is(chr(78), 'N', 'got the right char for 78');
is(chr(79), 'O', 'got the right char for 79');
is(chr(80), 'P', 'got the right char for 80');
is(chr(81), 'Q', 'got the right char for 81');
is(chr(82), 'R', 'got the right char for 82');
is(chr(83), 'S', 'got the right char for 83');
is(chr(84), 'T', 'got the right char for 84');
is(chr(85), 'U', 'got the right char for 85');
is(chr(86), 'V', 'got the right char for 86');
is(chr(87), 'W', 'got the right char for 87');
is(chr(88), 'X', 'got the right char for 88');
is(chr(89), 'Y', 'got the right char for 89');
is(chr(90), 'Z', 'got the right char for 90');
is(chr(91), '[', 'got the right char for 91');
is(chr(92), "\\", 'got the right char for 92');
is(chr(93), ']', 'got the right char for 93');
is(chr(94), '^', 'got the right char for 94');
is(chr(95), '_', 'got the right char for 95');
is(chr(96), '`', 'got the right char for 96');
is(chr(97), 'a', 'got the right char for 97');
is(chr(98), 'b', 'got the right char for 98');
is(chr(99), 'c', 'got the right char for 99');
is(chr(100), 'd', 'got the right char for 100');
is(chr(101), 'e', 'got the right char for 101');
is(chr(102), 'f', 'got the right char for 102');
is(chr(103), 'g', 'got the right char for 103');
is(chr(104), 'h', 'got the right char for 104');
is(chr(105), 'i', 'got the right char for 105');
is(chr(106), 'j', 'got the right char for 106');
is(chr(107), 'k', 'got the right char for 107');
is(chr(108), 'l', 'got the right char for 108');
is(chr(109), 'm', 'got the right char for 109');
is(chr(110), 'n', 'got the right char for 110');
is(chr(111), 'o', 'got the right char for 111');
is(chr(112), 'p', 'got the right char for 112');
is(chr(113), 'q', 'got the right char for 113');
is(chr(114), 'r', 'got the right char for 114');
is(chr(115), 's', 'got the right char for 115');
is(chr(116), 't', 'got the right char for 116');
is(chr(117), 'u', 'got the right char for 117');
is(chr(118), 'v', 'got the right char for 118');
is(chr(119), 'w', 'got the right char for 119');
is(chr(120), 'x', 'got the right char for 120');
is(chr(121), 'y', 'got the right char for 121');
is(chr(122), 'z', 'got the right char for 122');
is(chr(123), '{', 'got the right char for 123');
is(chr(124), '|', 'got the right char for 124');
is(chr(125), '}', 'got the right char for 125');
is(chr(126), '~', 'got the right char for 126');
