#!/usr/bin/pugs

# Tests for magic variables

use v6;
require Test;

plan(2);

# Tests for %*ENV

# it must not be empty at startup
ok(+%*ENV.keys, "ENV has keys");

# PATH is usually defined. But this test is not portable
ok(%*ENV{"PATH"}, "ENV has PATH");
