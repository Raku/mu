#!/usr/bin/pugs

use v6;
require Test;

plan(4);

ok(ucfirst "hello world" eq "Hello world", "simple");
ok(ucfirst "" eq "", "empty string");
ok(ucfirst "üüüü" eq "Üüüü", "umlaut");
ok(ucfirst "óóóó" eq "Óóóó", "accented chars");
