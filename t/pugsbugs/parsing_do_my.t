#!/usr/bin/pugs

use v6;
use Test;

=pod

pugs> do{my $a = 3; $a }
*** Undeclared variable: "$a"
    at <interactive> line 1, column 1-19

=cut

plan 2;

is(eval('do{my $a = 3; $a}'), 3, 'do{my $a = 3; $a} works');
is(do{1; my $a = 3; $a}, 3, 'bug workaround works');
