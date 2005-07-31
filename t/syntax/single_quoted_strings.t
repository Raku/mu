#!/usr/bin/pugs

use v6;
use Test;

plan 1;

# can parse 'test\\'
is('test\\', "test\\", "slashes in a single quoted string work correctly");
