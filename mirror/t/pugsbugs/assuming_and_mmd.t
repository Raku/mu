#!/usr/bin/pugs

use v6;
use Test;

plan 6;

multi sub testsub (Str $x, $y) { "Str" }
multi sub testsub (Int $x, $y) { "Int" }

is testsub("a_str", 42), "Str", "basic MMD works (1)";
is testsub(23,      42), "Int", "basic MMD works (2)";

is &testsub("a_str", 42), "Str", "basic MMD works with subrefs (1)";
is &testsub(23,      42), "Int", "basic MMD works with subrefs (2)";

is &testsub.assuming(x => "a_str")(42), "Str", "basic MMD works with assuming (1)", :todo<bug>;
is &testsub.assuming(x => 23)    .(42), "Int", "basic MMD works with assuming (2)";
