#!/usr/bin/pugs

use v6;
require Test;

# cant part 'test\\'
todo_eval_ok("'test\\\\'", "slashes in a single quoted string doesnt work correctly"); # unTODOme