#!/usr/bin/pugs

use v6;
use Test;

=kwid

`is primitive` seems to break named sub parameters.

=cut

plan 1;


multi sub testsub (Str $x, +$y) is primitive { $y }

ok testsub("moose", 42), 42, "named args in primive subs", :todo<bug>;
