#!/usr/bin/pugs

use v6;
require Test;

plan(3);

ok(uc "Hello World" eq "HELLO WORLD", "uc - simple");
ok(uc "åäö" eq "ÅÄÖ", "uc - some finnish non-achii chars");
ok(uc "ńóòúù" eq "ŃÓÒÚÙ", "uc - accented chars");

