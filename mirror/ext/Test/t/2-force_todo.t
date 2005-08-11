#!/usr/bin/pugs

use v6;
require Test;

plan 11;

force_todo(1, 3, 5, 7 .. 9, 11);

fail("This will fail, but will be forced-TODO by force_todo()"); 
pass("This will pass normally");
fail("This will fail, but will be forced-TODO by force_todo()");
pass("This will pass normally");
fail("This will TODO fail, and will be forced-TODO by force_todo()", :todo(1));
pass("This will pass normally");
fail("This will fail, and will be forced-TODO by force_todo()");
fail("This will fail, and will be forced-TODO by force_todo()");
fail("This will fail, and will be forced-TODO by force_todo()");
pass("This will pass normally");
fail("This will fail, and will be forced-TODO by force_todo()");
