#!/usr/bin/pugs

use v6;
require Test;

plan 4;

is lcfirst("HELLO WORLD"), "hELLO WORLD", "simple";
is lcfirst(""),            "",            "empty string";
is lcfirst("ÜÜÜÜ"),        "üÜÜÜ",        "umlaut";
is lcfirst("ÓÓÓÓŃ"),       "óÓÓÓŃ",       "accented chars";
