#!/usr/bin/pugs

use v6;
require Test;

plan(4);

ok(lcfirst "HELLO WORLD" eq "hELLO WORLD", "simple");
ok(lcfirst "" eq "", "empty string");
ok(lcfirst "ÜÜÜÜ" eq "üÜÜÜ", "umlaut");
ok(lcfirst "ŃÓÓÓÓ" eq "ńÓÓÓÓ", "accented chars");
