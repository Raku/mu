#!/usr/bin/perl
# Create primitives.
use strict; use warnings;

sub primitive {}
sub cl($) {}
sub js($) {}
sub p5($) {}
sub py($) {}
sub rb($) {}
sub scm($) {}


primitive 'stdout_print_strnum',1;
cl "(if (stringp a) (write-string a) (write a))";
js 'write a';
p5 'print $a';
py 'print a,';
rb 'print a';
scm 'display a';

