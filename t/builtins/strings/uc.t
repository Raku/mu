#!/usr/bin/pugs

use v6;
require Test;

plan 5;

is uc("Hello World"), "HELLO WORLD", "simple";
is uc(""),            "",            "empty string"; 
is uc("åäö"),         "ÅÄÖ",         "some finnish non-ascii chars";
is uc("óòúù"),        "ÓÒÚÙ",        "accented chars";

# Bug: GERMAN SHARP S ("ß") should uc() to "SS", but it doesn't
# Compare with: perl -we 'use utf8; print uc "ß"'
is uc("ß"),           "SS",          "uc() of non-ascii chars may result in two chars";
