#!/usr/bin/pugs

use v6;
require Test;

plan 7;

is lc("Hello World"),"hello world", "simple";
is lc("" ),          "",            "empty string";
is lc("ÅÄÖ"),        "åäö",         "some finnish non-ascii chars";
is lc("ÓÒÚÙ"),       "óòúù",        "accented chars";

# given does not return proper value yet
my $x;
given "Hello World" { $x = lc }
is $x, "hello world", "default operand";
$x = "Hello World";
is $x.lc, "hello world", "as method";
is "Hello World".lc, "hello world", "as string method";
