#!/usr/bin/pugs

use v6;
use Test;

plan 3;

=kwid

Difficulty using a rule in a method of Str.

=cut

class C is Str {
    method meth1 () {
	if ("bar" ~~ m:P5/[^a]/) {
	    "worked";
	} else {
	    "didnt";
	}
    }
}

is(C.new.meth1(),"worked",'m:P5/[^a]/ in method in C (is Str)');

say "-----";

class Str {
    method meth2 () {
	if ("bar" ~~ m:P5/[^a]/) {
	    "worked";
	} else {
	    "didnt";
	}
    }
}

is(Str.new.meth2(),"worked",'m:P5/[^a]/ in method in Str');

class Str {
    method meth3 () {
	if ("bar" ~~ m:P5/[a]/) {
	    "worked";
	} else {
	    "didnt";
	}
    }
}

is(Str.new.meth3(),"worked",'m:P5/[a]/ in method in Str');
