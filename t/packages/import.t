#!/usr/bin/pugs

use v6;
use Test;
plan 1;

BEGIN { @*INC.unshift('t/packages'); }

is(eval("use Import 'foo'; 123;"), 123, "import doesn't get called if it doesn't exist");
