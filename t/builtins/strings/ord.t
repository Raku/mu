#!/usr/bin/pugs

use v6;
require Test;

plan 95;

=pod

Basic tests for the ord() built-in

NOTE: these tests only deal with ASCII 

=cut

# What is the best way to test 0 through 31??

eval_is('ord(" ")', 32, 'got the right ord for  ');
eval_is('ord("!")', 33, 'got the right ord for !');
eval_is('ord("\"")', 34, 'got the right ord for "');
eval_is('ord("#")', 35, 'got the right ord for #');
eval_is('ord("$")', 36, 'got the right ord for $');
eval_is('ord("%")', 37, 'got the right ord for %');
eval_is('ord("&")', 38, 'got the right ord for &');
eval_is('ord("\'")', 39, 'got the right ord for (single quote)');
eval_is('ord("(")', 40, 'got the right ord for (');
eval_is('ord(")")', 41, 'got the right ord for )');
eval_is('ord("*")', 42, 'got the right ord for *');
eval_is('ord("+")', 43, 'got the right ord for +');
eval_is('ord(",")', 44, 'got the right ord for ,');
eval_is('ord("-")', 45, 'got the right ord for -');
eval_is('ord(".")', 46, 'got the right ord for .');
eval_is('ord("/")', 47, 'got the right ord for /');
eval_is('ord("0")', 48, 'got the right ord for 0');
eval_is('ord("1")', 49, 'got the right ord for 1');
eval_is('ord("2")', 50, 'got the right ord for 2');
eval_is('ord("3")', 51, 'got the right ord for 3');
eval_is('ord("4")', 52, 'got the right ord for 4');
eval_is('ord("5")', 53, 'got the right ord for 5');
eval_is('ord("6")', 54, 'got the right ord for 6');
eval_is('ord("7")', 55, 'got the right ord for 7');
eval_is('ord("8")', 56, 'got the right ord for 8');
eval_is('ord("9")', 57, 'got the right ord for 9');
eval_is('ord(":")', 58, 'got the right ord for :');
eval_is('ord(";")', 59, 'got the right ord for ;');
eval_is('ord("<")', 60, 'got the right ord for <');
eval_is('ord("=")', 61, 'got the right ord for =');
eval_is('ord(">")', 62, 'got the right ord for >');
eval_is('ord("?")', 63, 'got the right ord for ?');
eval_is('ord("@")', 64, 'got the right ord for @');
eval_is('ord("A")', 65, 'got the right ord for A');
eval_is('ord("B")', 66, 'got the right ord for B');
eval_is('ord("C")', 67, 'got the right ord for C');
eval_is('ord("D")', 68, 'got the right ord for D');
eval_is('ord("E")', 69, 'got the right ord for E');
eval_is('ord("F")', 70, 'got the right ord for F');
eval_is('ord("G")', 71, 'got the right ord for G');
eval_is('ord("H")', 72, 'got the right ord for H');
eval_is('ord("I")', 73, 'got the right ord for I');
eval_is('ord("J")', 74, 'got the right ord for J');
eval_is('ord("K")', 75, 'got the right ord for K');
eval_is('ord("L")', 76, 'got the right ord for L');
eval_is('ord("M")', 77, 'got the right ord for M');
eval_is('ord("N")', 78, 'got the right ord for N');
eval_is('ord("O")', 79, 'got the right ord for O');
eval_is('ord("P")', 80, 'got the right ord for P');
eval_is('ord("Q")', 81, 'got the right ord for Q');
eval_is('ord("R")', 82, 'got the right ord for R');
eval_is('ord("S")', 83, 'got the right ord for S');
eval_is('ord("T")', 84, 'got the right ord for T');
eval_is('ord("U")', 85, 'got the right ord for U');
eval_is('ord("V")', 86, 'got the right ord for V');
eval_is('ord("W")', 87, 'got the right ord for W');
eval_is('ord("X")', 88, 'got the right ord for X');
eval_is('ord("Y")', 89, 'got the right ord for Y');
eval_is('ord("Z")', 90, 'got the right ord for Z');
eval_is('ord("[")', 91, 'got the right ord for [');
eval_is('ord("\\\\")', 92, 'got the right ord for (backslash)');
eval_is('ord("]")', 93, 'got the right ord for ]');
eval_is('ord("^")', 94, 'got the right ord for ^');
eval_is('ord("_")', 95, 'got the right ord for _');
eval_is('ord("`")', 96, 'got the right ord for `');
eval_is('ord("a")', 97, 'got the right ord for a');
eval_is('ord("b")', 98, 'got the right ord for b');
eval_is('ord("c")', 99, 'got the right ord for c');
eval_is('ord("d")', 100, 'got the right ord for d');
eval_is('ord("e")', 101, 'got the right ord for e');
eval_is('ord("f")', 102, 'got the right ord for f');
eval_is('ord("g")', 103, 'got the right ord for g');
eval_is('ord("h")', 104, 'got the right ord for h');
eval_is('ord("i")', 105, 'got the right ord for i');
eval_is('ord("j")', 106, 'got the right ord for j');
eval_is('ord("k")', 107, 'got the right ord for k');
eval_is('ord("l")', 108, 'got the right ord for l');
eval_is('ord("m")', 109, 'got the right ord for m');
eval_is('ord("n")', 110, 'got the right ord for n');
eval_is('ord("o")', 111, 'got the right ord for o');
eval_is('ord("p")', 112, 'got the right ord for p');
eval_is('ord("q")', 113, 'got the right ord for q');
eval_is('ord("r")', 114, 'got the right ord for r');
eval_is('ord("s")', 115, 'got the right ord for s');
eval_is('ord("t")', 116, 'got the right ord for t');
eval_is('ord("u")', 117, 'got the right ord for u');
eval_is('ord("v")', 118, 'got the right ord for v');
eval_is('ord("w")', 119, 'got the right ord for w');
eval_is('ord("x")', 120, 'got the right ord for x');
eval_is('ord("y")', 121, 'got the right ord for y');
eval_is('ord("z")', 122, 'got the right ord for z');
eval_is('ord(\'{\')', 123, 'got the right ord for {');
eval_is('ord("|")', 124, 'got the right ord for |');
eval_is('ord("}")', 125, 'got the right ord for }');
eval_is('ord("~")', 126, 'got the right ord for ~');