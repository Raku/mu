#!/usr/bin/pugs

use v6;
require Test;

plan(3);

ok(lc "Hello World" eq "hello world", "lc - simple");
ok(lc "ÅÄÖ" eq "åäö", "lc - some finnish non-achii chars");
ok(lc "ŃÓÒÚÙ" eq "ńóòúù", "lc - accented chars");

