#!/usr/bin/pugs

use v6;

require Test;

plan 2;

is($?LINE, 9, '$?LINE works');
is($?FILE, 't/magicals/vars.t', '$?FILE works');
