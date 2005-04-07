#!/usr/bin/pugs

use v6;
require Test;

plan 8;

is uc("Hello World"), "HELLO WORLD", "simple";
is uc(""),            "",            "empty string"; 
is uc("åäö"),         "ÅÄÖ",         "some finnish non-ascii chars";
is uc("óòúù"),        "ÓÒÚÙ",        "accented chars";

# given does not return proper value yet
my $x;
given "Hello World" { $x = uc }
is $x, "HELLO WORLD", "default operand";
$x = "Hello World";
is $x.uc, "HELLO WORLD", "as method";
is "Hello World".uc, "HELLO WORLD", "as string method";

# Bug: GERMAN SHARP S ("ß") should uc() to "SS", but it doesn't
# Compare with: perl -we 'use utf8; print uc "ß"'
is uc("ß"),           "SS",          "uc() of non-ascii chars may result in two chars";
