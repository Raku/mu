#!/usr/bin/pugs
use v6;
require Test;

plan 1;

my $a = '';

ok ((eval '("$a.print"); "1";'), "parsing method call in a string");
