#!/usr/bin/pugs

use v6;
require Test;

# cant part 'test\\'
eval_ok("'test\\\\'", "slashes in a single quoted string doesnt work correctly");