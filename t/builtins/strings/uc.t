#!/usr/bin/pugs

use v6;
require Test;

plan(5);

ok(uc "Hello World" eq "HELLO WORLD", "simple");
ok(uc "" eq "", "empty string"); 
ok(uc "åäö" eq "ÅÄÖ", "some finnish non-ascii chars");
ok(uc "óòúù" eq "ÓÒÚÙ", "accented chars");

# Bug: GERMAN SHARP S ("ß") should uc() to "SS", but it doesn't
# Compare with: perl -we 'use utf8; print uc "ß"'
todo_ok(uc "ß" eq "SS", "uc() of non-ascii chars may result in two chars"); # unTODOme
