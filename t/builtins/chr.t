#!/usr/bin/pugs

use v6;
require Test;

plan 95;

=pod

Basic tests for the char() builtin

NOTE: these tests only deal with ASCII 

=cut

# What is the best way to test 0 through 31??

todo_eval_is('chr(32)', ' ', 'got the right char for 32');
todo_eval_is('chr(33)', '!', 'got the right char for 33');
todo_eval_is('chr(34)', '"', 'got the right char for 34'); #" 
todo_eval_is('chr(35)', '#', 'got the right char for 35');
todo_eval_is('chr(36)', '$', 'got the right char for 36');
todo_eval_is('chr(37)', '%', 'got the right char for 37');
todo_eval_is('chr(38)', '&', 'got the right char for 38');
todo_eval_is('chr(39)', "'", 'got the right char for 39');
todo_eval_is('chr(40)', '(', 'got the right char for 40');
todo_eval_is('chr(41)', ')', 'got the right char for 41');
todo_eval_is('chr(42)', '*', 'got the right char for 42');
todo_eval_is('chr(43)', '+', 'got the right char for 43');
todo_eval_is('chr(44)', ',', 'got the right char for 44');
todo_eval_is('chr(45)', '-', 'got the right char for 45');
todo_eval_is('chr(46)', '.', 'got the right char for 46');
todo_eval_is('chr(47)', '/', 'got the right char for 47');
todo_eval_is('chr(48)', '0', 'got the right char for 48');
todo_eval_is('chr(49)', '1', 'got the right char for 49');
todo_eval_is('chr(50)', '2', 'got the right char for 50');
todo_eval_is('chr(51)', '3', 'got the right char for 51');
todo_eval_is('chr(52)', '4', 'got the right char for 52');
todo_eval_is('chr(53)', '5', 'got the right char for 53');
todo_eval_is('chr(54)', '6', 'got the right char for 54');
todo_eval_is('chr(55)', '7', 'got the right char for 55');
todo_eval_is('chr(56)', '8', 'got the right char for 56');
todo_eval_is('chr(57)', '9', 'got the right char for 57');
todo_eval_is('chr(58)', ':', 'got the right char for 58');
todo_eval_is('chr(59)', ';', 'got the right char for 59');
todo_eval_is('chr(60)', '<', 'got the right char for 60');
todo_eval_is('chr(61)', '=', 'got the right char for 61');
todo_eval_is('chr(62)', '>', 'got the right char for 62');
todo_eval_is('chr(63)', '?', 'got the right char for 63');
todo_eval_is('chr(64)', '@', 'got the right char for 64');
todo_eval_is('chr(65)', 'A', 'got the right char for 65');
todo_eval_is('chr(66)', 'B', 'got the right char for 66');
todo_eval_is('chr(67)', 'C', 'got the right char for 67');
todo_eval_is('chr(68)', 'D', 'got the right char for 68');
todo_eval_is('chr(69)', 'E', 'got the right char for 69');
todo_eval_is('chr(70)', 'F', 'got the right char for 70');
todo_eval_is('chr(71)', 'G', 'got the right char for 71');
todo_eval_is('chr(72)', 'H', 'got the right char for 72');
todo_eval_is('chr(73)', 'I', 'got the right char for 73');
todo_eval_is('chr(74)', 'J', 'got the right char for 74');
todo_eval_is('chr(75)', 'K', 'got the right char for 75');
todo_eval_is('chr(76)', 'L', 'got the right char for 76');
todo_eval_is('chr(77)', 'M', 'got the right char for 77');
todo_eval_is('chr(78)', 'N', 'got the right char for 78');
todo_eval_is('chr(79)', 'O', 'got the right char for 79');
todo_eval_is('chr(80)', 'P', 'got the right char for 80');
todo_eval_is('chr(81)', 'Q', 'got the right char for 81');
todo_eval_is('chr(82)', 'R', 'got the right char for 82');
todo_eval_is('chr(83)', 'S', 'got the right char for 83');
todo_eval_is('chr(84)', 'T', 'got the right char for 84');
todo_eval_is('chr(85)', 'U', 'got the right char for 85');
todo_eval_is('chr(86)', 'V', 'got the right char for 86');
todo_eval_is('chr(87)', 'W', 'got the right char for 87');
todo_eval_is('chr(88)', 'X', 'got the right char for 88');
todo_eval_is('chr(89)', 'Y', 'got the right char for 89');
todo_eval_is('chr(90)', 'Z', 'got the right char for 90');
todo_eval_is('chr(91)', '[', 'got the right char for 91');
todo_eval_is('chr(92)', "\\", 'got the right char for 92');
todo_eval_is('chr(93)', ']', 'got the right char for 93');
todo_eval_is('chr(94)', '^', 'got the right char for 94');
todo_eval_is('chr(95)', '_', 'got the right char for 95');
todo_eval_is('chr(96)', '`', 'got the right char for 96');
todo_eval_is('chr(97)', 'a', 'got the right char for 97');
todo_eval_is('chr(98)', 'b', 'got the right char for 98');
todo_eval_is('chr(99)', 'c', 'got the right char for 99');
todo_eval_is('chr(100)', 'd', 'got the right char for 100');
todo_eval_is('chr(101)', 'e', 'got the right char for 101');
todo_eval_is('chr(102)', 'f', 'got the right char for 102');
todo_eval_is('chr(103)', 'g', 'got the right char for 103');
todo_eval_is('chr(104)', 'h', 'got the right char for 104');
todo_eval_is('chr(105)', 'i', 'got the right char for 105');
todo_eval_is('chr(106)', 'j', 'got the right char for 106');
todo_eval_is('chr(107)', 'k', 'got the right char for 107');
todo_eval_is('chr(108)', 'l', 'got the right char for 108');
todo_eval_is('chr(109)', 'm', 'got the right char for 109');
todo_eval_is('chr(110)', 'n', 'got the right char for 110');
todo_eval_is('chr(111)', 'o', 'got the right char for 111');
todo_eval_is('chr(112)', 'p', 'got the right char for 112');
todo_eval_is('chr(113)', 'q', 'got the right char for 113');
todo_eval_is('chr(114)', 'r', 'got the right char for 114');
todo_eval_is('chr(115)', 's', 'got the right char for 115');
todo_eval_is('chr(116)', 't', 'got the right char for 116');
todo_eval_is('chr(117)', 'u', 'got the right char for 117');
todo_eval_is('chr(118)', 'v', 'got the right char for 118');
todo_eval_is('chr(119)', 'w', 'got the right char for 119');
todo_eval_is('chr(120)', 'x', 'got the right char for 120');
todo_eval_is('chr(121)', 'y', 'got the right char for 121');
todo_eval_is('chr(122)', 'z', 'got the right char for 122');
todo_eval_is('chr(123)', '{', 'got the right char for 123');
todo_eval_is('chr(124)', '|', 'got the right char for 124');
todo_eval_is('chr(125)', '}', 'got the right char for 125');
todo_eval_is('chr(126)', '~', 'got the right char for 126');