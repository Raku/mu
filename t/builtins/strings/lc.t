#!/usr/bin/pugs

use v6;
require Test;

plan 4;

is lc("Hello World"),"hello world", "simple";
is lc("" ),          "",            "empty string";
is lc("ÅÄÖ"),        "åäö",         "some finnish non-ascii chars";
is lc("ÓÒÚÙ"),       "óòúù",        "accented chars";
