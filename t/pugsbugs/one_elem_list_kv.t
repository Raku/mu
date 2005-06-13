#!/usr/bin/pugs

use v6;
use Test;

plan(6);

fail("(1).kv hangs forever");
fail("('a').kv hangs forever");

# once it works, just remove the previous 2 lines, set plan to 4 and uncomment the code bits below:

# (1).kv works correctly
my @a = ();
# @a = (1).kv; # commented out - hangs forever
is(@a[0],0, "first element is 0");
is(@a[1],1, "second element is 1");

# ('a').kv works correctly
# @a = ('a').kv; # commented out - hangs forever
is(@a[0],0, "first element is 0");
is(@a[1],'a', "second element is 'a'");

