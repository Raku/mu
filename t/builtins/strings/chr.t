#!/usr/bin/pugs

use v6;
require Test;

plan 95;

=pod

Basic tests for the char() builtin

NOTE: these tests only deal with ASCII 

=cut

# What is the best way to test 0 through 31??

eval_is('chr(32)', ' ', 'got the right char for 32');
eval_is('chr(33)', '!', 'got the right char for 33');
eval_is('chr(34)', '"', 'got the right char for 34'); #" 
eval_is('chr(35)', '#', 'got the right char for 35');
eval_is('chr(36)', '$', 'got the right char for 36');
eval_is('chr(37)', '%', 'got the right char for 37');
eval_is('chr(38)', '&', 'got the right char for 38');
eval_is('chr(39)', "'", 'got the right char for 39');
eval_is('chr(40)', '(', 'got the right char for 40');
eval_is('chr(41)', ')', 'got the right char for 41');
eval_is('chr(42)', '*', 'got the right char for 42');
eval_is('chr(43)', '+', 'got the right char for 43');
eval_is('chr(44)', ',', 'got the right char for 44');
eval_is('chr(45)', '-', 'got the right char for 45');
eval_is('chr(46)', '.', 'got the right char for 46');
eval_is('chr(47)', '/', 'got the right char for 47');
eval_is('chr(48)', '0', 'got the right char for 48');
eval_is('chr(49)', '1', 'got the right char for 49');
eval_is('chr(50)', '2', 'got the right char for 50');
eval_is('chr(51)', '3', 'got the right char for 51');
eval_is('chr(52)', '4', 'got the right char for 52');
eval_is('chr(53)', '5', 'got the right char for 53');
eval_is('chr(54)', '6', 'got the right char for 54');
eval_is('chr(55)', '7', 'got the right char for 55');
eval_is('chr(56)', '8', 'got the right char for 56');
eval_is('chr(57)', '9', 'got the right char for 57');
eval_is('chr(58)', ':', 'got the right char for 58');
eval_is('chr(59)', ';', 'got the right char for 59');
eval_is('chr(60)', '<', 'got the right char for 60');
eval_is('chr(61)', '=', 'got the right char for 61');
eval_is('chr(62)', '>', 'got the right char for 62');
eval_is('chr(63)', '?', 'got the right char for 63');
eval_is('chr(64)', '@', 'got the right char for 64');
eval_is('chr(65)', 'A', 'got the right char for 65');
eval_is('chr(66)', 'B', 'got the right char for 66');
eval_is('chr(67)', 'C', 'got the right char for 67');
eval_is('chr(68)', 'D', 'got the right char for 68');
eval_is('chr(69)', 'E', 'got the right char for 69');
eval_is('chr(70)', 'F', 'got the right char for 70');
eval_is('chr(71)', 'G', 'got the right char for 71');
eval_is('chr(72)', 'H', 'got the right char for 72');
eval_is('chr(73)', 'I', 'got the right char for 73');
eval_is('chr(74)', 'J', 'got the right char for 74');
eval_is('chr(75)', 'K', 'got the right char for 75');
eval_is('chr(76)', 'L', 'got the right char for 76');
eval_is('chr(77)', 'M', 'got the right char for 77');
eval_is('chr(78)', 'N', 'got the right char for 78');
eval_is('chr(79)', 'O', 'got the right char for 79');
eval_is('chr(80)', 'P', 'got the right char for 80');
eval_is('chr(81)', 'Q', 'got the right char for 81');
eval_is('chr(82)', 'R', 'got the right char for 82');
eval_is('chr(83)', 'S', 'got the right char for 83');
eval_is('chr(84)', 'T', 'got the right char for 84');
eval_is('chr(85)', 'U', 'got the right char for 85');
eval_is('chr(86)', 'V', 'got the right char for 86');
eval_is('chr(87)', 'W', 'got the right char for 87');
eval_is('chr(88)', 'X', 'got the right char for 88');
eval_is('chr(89)', 'Y', 'got the right char for 89');
eval_is('chr(90)', 'Z', 'got the right char for 90');
eval_is('chr(91)', '[', 'got the right char for 91');
eval_is('chr(92)', "\\", 'got the right char for 92');
eval_is('chr(93)', ']', 'got the right char for 93');
eval_is('chr(94)', '^', 'got the right char for 94');
eval_is('chr(95)', '_', 'got the right char for 95');
eval_is('chr(96)', '`', 'got the right char for 96');
eval_is('chr(97)', 'a', 'got the right char for 97');
eval_is('chr(98)', 'b', 'got the right char for 98');
eval_is('chr(99)', 'c', 'got the right char for 99');
eval_is('chr(100)', 'd', 'got the right char for 100');
eval_is('chr(101)', 'e', 'got the right char for 101');
eval_is('chr(102)', 'f', 'got the right char for 102');
eval_is('chr(103)', 'g', 'got the right char for 103');
eval_is('chr(104)', 'h', 'got the right char for 104');
eval_is('chr(105)', 'i', 'got the right char for 105');
eval_is('chr(106)', 'j', 'got the right char for 106');
eval_is('chr(107)', 'k', 'got the right char for 107');
eval_is('chr(108)', 'l', 'got the right char for 108');
eval_is('chr(109)', 'm', 'got the right char for 109');
eval_is('chr(110)', 'n', 'got the right char for 110');
eval_is('chr(111)', 'o', 'got the right char for 111');
eval_is('chr(112)', 'p', 'got the right char for 112');
eval_is('chr(113)', 'q', 'got the right char for 113');
eval_is('chr(114)', 'r', 'got the right char for 114');
eval_is('chr(115)', 's', 'got the right char for 115');
eval_is('chr(116)', 't', 'got the right char for 116');
eval_is('chr(117)', 'u', 'got the right char for 117');
eval_is('chr(118)', 'v', 'got the right char for 118');
eval_is('chr(119)', 'w', 'got the right char for 119');
eval_is('chr(120)', 'x', 'got the right char for 120');
eval_is('chr(121)', 'y', 'got the right char for 121');
eval_is('chr(122)', 'z', 'got the right char for 122');
eval_is('chr(123)', '{', 'got the right char for 123');
eval_is('chr(124)', '|', 'got the right char for 124');
eval_is('chr(125)', '}', 'got the right char for 125');
eval_is('chr(126)', '~', 'got the right char for 126');