#!/usr/bin/pugs

use v6;
use Test;

# This test is primarily aimed at PIL2JS.
# In conditionals, or, to be more exact, in all things using PIL2JS.cps2normal,
# exit() did call all END blocks, but the control flow was resumed afterwards.
# This is now fixed, but it's still good to have a test for it.

plan 1;

if 1 {
  pass;
  exit;
}

ok 0, "exit() in if didn't work";
