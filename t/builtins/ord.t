#!/usr/bin/pugs

use v6;
require Test;

plan 95;

=pod

Basic tests for the ord() built-in

NOTE: these tests only deal with ASCII 

=cut

# What is the best way to test 0 through 31??

todo_eval_is('ord(" ")', 32, 'got the right ord for  ');
todo_eval_is('ord("!")', 33, 'got the right ord for !');
todo_eval_is('ord("\"")', 34, 'got the right ord for "');
todo_eval_is('ord("#")', 35, 'got the right ord for #');
todo_eval_is('ord("$")', 36, 'got the right ord for $');
todo_eval_is('ord("%")', 37, 'got the right ord for %');
todo_eval_is('ord("&")', 38, 'got the right ord for &');
todo_eval_is('ord("\'")', 39, 'got the right ord for (single quote)');
todo_eval_is('ord("(")', 40, 'got the right ord for (');
todo_eval_is('ord(")")', 41, 'got the right ord for )');
todo_eval_is('ord("*")', 42, 'got the right ord for *');
todo_eval_is('ord("+")', 43, 'got the right ord for +');
todo_eval_is('ord(",")', 44, 'got the right ord for ,');
todo_eval_is('ord("-")', 45, 'got the right ord for -');
todo_eval_is('ord(".")', 46, 'got the right ord for .');
todo_eval_is('ord("/")', 47, 'got the right ord for /');
todo_eval_is('ord("0")', 48, 'got the right ord for 0');
todo_eval_is('ord("1")', 49, 'got the right ord for 1');
todo_eval_is('ord("2")', 50, 'got the right ord for 2');
todo_eval_is('ord("3")', 51, 'got the right ord for 3');
todo_eval_is('ord("4")', 52, 'got the right ord for 4');
todo_eval_is('ord("5")', 53, 'got the right ord for 5');
todo_eval_is('ord("6")', 54, 'got the right ord for 6');
todo_eval_is('ord("7")', 55, 'got the right ord for 7');
todo_eval_is('ord("8")', 56, 'got the right ord for 8');
todo_eval_is('ord("9")', 57, 'got the right ord for 9');
todo_eval_is('ord(":")', 58, 'got the right ord for :');
todo_eval_is('ord(";")', 59, 'got the right ord for ;');
todo_eval_is('ord("<")', 60, 'got the right ord for <');
todo_eval_is('ord("=")', 61, 'got the right ord for =');
todo_eval_is('ord(">")', 62, 'got the right ord for >');
todo_eval_is('ord("?")', 63, 'got the right ord for ?');
todo_eval_is('ord("@")', 64, 'got the right ord for @');
todo_eval_is('ord("A")', 65, 'got the right ord for A');
todo_eval_is('ord("B")', 66, 'got the right ord for B');
todo_eval_is('ord("C")', 67, 'got the right ord for C');
todo_eval_is('ord("D")', 68, 'got the right ord for D');
todo_eval_is('ord("E")', 69, 'got the right ord for E');
todo_eval_is('ord("F")', 70, 'got the right ord for F');
todo_eval_is('ord("G")', 71, 'got the right ord for G');
todo_eval_is('ord("H")', 72, 'got the right ord for H');
todo_eval_is('ord("I")', 73, 'got the right ord for I');
todo_eval_is('ord("J")', 74, 'got the right ord for J');
todo_eval_is('ord("K")', 75, 'got the right ord for K');
todo_eval_is('ord("L")', 76, 'got the right ord for L');
todo_eval_is('ord("M")', 77, 'got the right ord for M');
todo_eval_is('ord("N")', 78, 'got the right ord for N');
todo_eval_is('ord("O")', 79, 'got the right ord for O');
todo_eval_is('ord("P")', 80, 'got the right ord for P');
todo_eval_is('ord("Q")', 81, 'got the right ord for Q');
todo_eval_is('ord("R")', 82, 'got the right ord for R');
todo_eval_is('ord("S")', 83, 'got the right ord for S');
todo_eval_is('ord("T")', 84, 'got the right ord for T');
todo_eval_is('ord("U")', 85, 'got the right ord for U');
todo_eval_is('ord("V")', 86, 'got the right ord for V');
todo_eval_is('ord("W")', 87, 'got the right ord for W');
todo_eval_is('ord("X")', 88, 'got the right ord for X');
todo_eval_is('ord("Y")', 89, 'got the right ord for Y');
todo_eval_is('ord("Z")', 90, 'got the right ord for Z');
todo_eval_is('ord("[")', 91, 'got the right ord for [');
todo_eval_is('ord("\\\\")', 92, 'got the right ord for (backslash)');
todo_eval_is('ord("]")', 93, 'got the right ord for ]');
todo_eval_is('ord("^")', 94, 'got the right ord for ^');
todo_eval_is('ord("_")', 95, 'got the right ord for _');
todo_eval_is('ord("`")', 96, 'got the right ord for `');
todo_eval_is('ord("a")', 97, 'got the right ord for a');
todo_eval_is('ord("b")', 98, 'got the right ord for b');
todo_eval_is('ord("c")', 99, 'got the right ord for c');
todo_eval_is('ord("d")', 100, 'got the right ord for d');
todo_eval_is('ord("e")', 101, 'got the right ord for e');
todo_eval_is('ord("f")', 102, 'got the right ord for f');
todo_eval_is('ord("g")', 103, 'got the right ord for g');
todo_eval_is('ord("h")', 104, 'got the right ord for h');
todo_eval_is('ord("i")', 105, 'got the right ord for i');
todo_eval_is('ord("j")', 106, 'got the right ord for j');
todo_eval_is('ord("k")', 107, 'got the right ord for k');
todo_eval_is('ord("l")', 108, 'got the right ord for l');
todo_eval_is('ord("m")', 109, 'got the right ord for m');
todo_eval_is('ord("n")', 110, 'got the right ord for n');
todo_eval_is('ord("o")', 111, 'got the right ord for o');
todo_eval_is('ord("p")', 112, 'got the right ord for p');
todo_eval_is('ord("q")', 113, 'got the right ord for q');
todo_eval_is('ord("r")', 114, 'got the right ord for r');
todo_eval_is('ord("s")', 115, 'got the right ord for s');
todo_eval_is('ord("t")', 116, 'got the right ord for t');
todo_eval_is('ord("u")', 117, 'got the right ord for u');
todo_eval_is('ord("v")', 118, 'got the right ord for v');
todo_eval_is('ord("w")', 119, 'got the right ord for w');
todo_eval_is('ord("x")', 120, 'got the right ord for x');
todo_eval_is('ord("y")', 121, 'got the right ord for y');
todo_eval_is('ord("z")', 122, 'got the right ord for z');
todo_eval_is('ord(\'{\')', 123, 'got the right ord for {');
todo_eval_is('ord("|")', 124, 'got the right ord for |');
todo_eval_is('ord("}")', 125, 'got the right ord for }');
todo_eval_is('ord("~")', 126, 'got the right ord for ~');