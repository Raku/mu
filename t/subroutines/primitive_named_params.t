#!/usr/bin/pugs

use v6;
use Test;

=kwid

`is primitive` seems to break named sub parameters.

=cut

plan 1;


multi sub testsub (Str $x, :$y) is primitive { "$x $y" }

is testsub("moose", y => 42), "moose 42", "named args in primitive subs";
