#!/usr/bin/pugs

use v6;
require Test;

# can parse 'test\\'
eval_ok("'test\\\\'", "slashes in a single quoted string work correctly");
