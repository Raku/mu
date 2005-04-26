#!/usr/bin/pugs

use v6;
use Test;

plan 4;

is ucfirst("hello world"), "Hello world", "simple";
is ucfirst(""),            "",            "empty string";
is ucfirst("üüüü"),        "Üüüü",        "umlaut";
is ucfirst("óóóó"),        "Óóóó",        "accented chars";
