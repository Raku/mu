#!/usr/bin/pugs

use v6;
use Test;

plan 4;

# L<S29/"Perl6::Str" /lcfirst/>

is lcfirst("HELLO WORLD"), "hELLO WORLD", "simple";
is lcfirst(""),            "",            "empty string";
is lcfirst("ÜÜÜÜ"),        "üÜÜÜ",        "umlaut";
is lcfirst("ÓÓÓÓŃ"),       "óÓÓÓŃ",       "accented chars";
