#!/usr/bin/env perl
# Run a kp6 script by filtering it through the kp6->lisp compiler and
# run sbcl on the result

use strict;
use warnings;

# This is a poor heuristic
my $prog = shift; # kp6 program

my $cmd;
if (defined $prog && length $prog){
	$cmd = "$^X script/kp6 -lisp <$prog | ecl | $^X -ne 'do { s/^KP6-LISP-USER> //; print } unless 1 .. /bytecompiled/'";
} else {
	$cmd = "$^X script/kp6 -lisp | ecl | $^X -ne 'do { s/^KP6-LISP-USER> //; print } unless 1 .. /bytecompiled/'";
}

system $cmd;
