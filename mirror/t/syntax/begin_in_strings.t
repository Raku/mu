#!/usr/bin/pugs

use v6;
use Test;

plan 1;

is "abc{ BEGIN { 3 } }def", "abc3def", 'BEGIN {} works in double quoted strings';
