#!/usr/bin/pugs

# Tests for magic variables

use v6;
require Test;

plan(1);

# Tests for %*ENV

# it must not be empty at startup
ok(+%*ENV.keys, "ENV has keys");
