#!/usr/bin/pugs

use v6;
require Test;

fail("This will fail, but will be forced-TODO");
pass("This will pass normally");
fail("This will fail, but will be forced-TODO");
pass("This will pass normally");
todo_fail("This will TODO fail, and will be forced-TODO");