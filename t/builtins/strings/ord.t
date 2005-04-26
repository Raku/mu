#!/usr/bin/pugs

use v6;
use Test;

plan 95;

=pod

Basic tests for the ord() built-in

NOTE: these tests only deal with ASCII

=cut

# What is the best way to test 0 through 31??

is(ord(" "), 32, 'got the right ord for  ');
is(ord("!"), 33, 'got the right ord for !');
is(ord("\""), 34, 'got the right ord for "');
is(ord("#"), 35, 'got the right ord for #');
is(ord("$"), 36, 'got the right ord for $');
is(ord("%"), 37, 'got the right ord for %');
is(ord("&"), 38, 'got the right ord for &');
is(ord("\'"), 39, 'got the right ord for (single quote)');
is(ord("("), 40, 'got the right ord for (');
is(ord(")"), 41, 'got the right ord for )');
is(ord("*"), 42, 'got the right ord for *');
is(ord("+"), 43, 'got the right ord for +');
is(ord(","), 44, 'got the right ord for ,');
is(ord("-"), 45, 'got the right ord for -');
is(ord("."), 46, 'got the right ord for .');
is(ord("/"), 47, 'got the right ord for /');
is(ord("0"), 48, 'got the right ord for 0');
is(ord("1"), 49, 'got the right ord for 1');
is(ord("2"), 50, 'got the right ord for 2');
is(ord("3"), 51, 'got the right ord for 3');
is(ord("4"), 52, 'got the right ord for 4');
is(ord("5"), 53, 'got the right ord for 5');
is(ord("6"), 54, 'got the right ord for 6');
is(ord("7"), 55, 'got the right ord for 7');
is(ord("8"), 56, 'got the right ord for 8');
is(ord("9"), 57, 'got the right ord for 9');
is(ord(":"), 58, 'got the right ord for :');
is(ord(";"), 59, 'got the right ord for ;');
is(ord("<"), 60, 'got the right ord for <');
is(ord("="), 61, 'got the right ord for =');
is(ord(">"), 62, 'got the right ord for >');
is(ord("?"), 63, 'got the right ord for ?');
is(ord("@"), 64, 'got the right ord for @');
is(ord("A"), 65, 'got the right ord for A');
is(ord("B"), 66, 'got the right ord for B');
is(ord("C"), 67, 'got the right ord for C');
is(ord("D"), 68, 'got the right ord for D');
is(ord("E"), 69, 'got the right ord for E');
is(ord("F"), 70, 'got the right ord for F');
is(ord("G"), 71, 'got the right ord for G');
is(ord("H"), 72, 'got the right ord for H');
is(ord("I"), 73, 'got the right ord for I');
is(ord("J"), 74, 'got the right ord for J');
is(ord("K"), 75, 'got the right ord for K');
is(ord("L"), 76, 'got the right ord for L');
is(ord("M"), 77, 'got the right ord for M');
is(ord("N"), 78, 'got the right ord for N');
is(ord("O"), 79, 'got the right ord for O');
is(ord("P"), 80, 'got the right ord for P');
is(ord("Q"), 81, 'got the right ord for Q');
is(ord("R"), 82, 'got the right ord for R');
is(ord("S"), 83, 'got the right ord for S');
is(ord("T"), 84, 'got the right ord for T');
is(ord("U"), 85, 'got the right ord for U');
is(ord("V"), 86, 'got the right ord for V');
is(ord("W"), 87, 'got the right ord for W');
is(ord("X"), 88, 'got the right ord for X');
is(ord("Y"), 89, 'got the right ord for Y');
is(ord("Z"), 90, 'got the right ord for Z');
is(ord("["), 91, 'got the right ord for [');
is(ord("\\\\"), 92, 'got the right ord for (backslash)');
is(ord("]"), 93, 'got the right ord for ]');
is(ord("^"), 94, 'got the right ord for ^');
is(ord("_"), 95, 'got the right ord for _');
is(ord("`"), 96, 'got the right ord for `');
is(ord("a"), 97, 'got the right ord for a');
is(ord("b"), 98, 'got the right ord for b');
is(ord("c"), 99, 'got the right ord for c');
is(ord("d"), 100, 'got the right ord for d');
is(ord("e"), 101, 'got the right ord for e');
is(ord("f"), 102, 'got the right ord for f');
is(ord("g"), 103, 'got the right ord for g');
is(ord("h"), 104, 'got the right ord for h');
is(ord("i"), 105, 'got the right ord for i');
is(ord("j"), 106, 'got the right ord for j');
is(ord("k"), 107, 'got the right ord for k');
is(ord("l"), 108, 'got the right ord for l');
is(ord("m"), 109, 'got the right ord for m');
is(ord("n"), 110, 'got the right ord for n');
is(ord("o"), 111, 'got the right ord for o');
is(ord("p"), 112, 'got the right ord for p');
is(ord("q"), 113, 'got the right ord for q');
is(ord("r"), 114, 'got the right ord for r');
is(ord("s"), 115, 'got the right ord for s');
is(ord("t"), 116, 'got the right ord for t');
is(ord("u"), 117, 'got the right ord for u');
is(ord("v"), 118, 'got the right ord for v');
is(ord("w"), 119, 'got the right ord for w');
is(ord("x"), 120, 'got the right ord for x');
is(ord("y"), 121, 'got the right ord for y');
is(ord("z"), 122, 'got the right ord for z');
is(ord('{'), 123, 'got the right ord for {');
is(ord("|"), 124, 'got the right ord for |');
is(ord("}"), 125, 'got the right ord for }');
is(ord("~"), 126, 'got the right ord for ~');
