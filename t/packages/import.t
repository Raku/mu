#!/usr/bin/pugs

use v6;
use Test;
plan 1;

is(eval("use Import 'foo';"), 501, "import doesn't get called if it doesn't exist");
