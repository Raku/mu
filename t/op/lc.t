#!/usr/bin/pugs

use v6;
require Test;

plan(4);

ok(lc "Hello World" eq "hello world", "simple");
ok(lc "" eq "", "empty string");
ok(lc "ÅÄÖ" eq "åäö", "some finnish non-achii chars");
is(lc "ÓÒÚÙ", "óòúù", "accented chars");

