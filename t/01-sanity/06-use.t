#!/usr/bin/pugs

# Checking that testing is sane: use

use v6;

# We've to output the TAP header at begin time to ensure it is outputted,
# as the use() below might not work, causing this program to not even compile,
# causing the TAP header to not be printed.
BEGIN { say '1..1' }

# We try to load an arbitrary module.
use lib ".";

say 'ok 1';
