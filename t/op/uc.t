#!/usr/bin/pugs

use v6;
require Test;

plan(4);

ok(uc "Hello World" eq "HELLO WORLD", "simple");
ok(uc "" eq "", "empty string"); 
ok(uc "åäö" eq "ÅÄÖ", "some finnish non-achii chars");
ok(uc "ńóòúù" eq "ŃÓÒÚÙ", "accented chars");

