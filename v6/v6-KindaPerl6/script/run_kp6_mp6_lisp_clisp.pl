#!/usr/bin/env perl
# Run a kp6 script by filtering it through the kp6->lisp compiler and
# run clisp on the result

use strict;
use warnings;

# This is a poor heuristic
my $prog = shift; # kp6 program

my $cmd;
if (defined $prog && length $prog){
	$cmd = "$^X script/kp6 -lisp <$prog | clisp -q -q -on-error exit | $^X -ne 'print unless 1..m{4\]> (.*)(?{print\$1,\$/})}i'";
} else {
	$cmd = "$^X script/kp6 -lisp | clisp -q -q -on-error exit | $^X -ne 'print unless 1..m{4\]> (.*)(?{print\$1,\$/})}i'";
}

system $cmd;
