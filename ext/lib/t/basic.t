#!/usr/bin/pugs

use v6;
require Test;

plan 5;

use_ok('lib');

my $inc_count = +@*INC;

import('lib': '../lib');

is($inc_count + 1, +@*INC, 'we have added one path to the @*INC');
is(@*INC[0], '../lib', '... and it is what and where we expected it to be');

# now check adding duplicates

import('lib': '../lib/My/Module', '../lib/My/Module');

is($inc_count + 2, +@*INC, 'we have added only one new path to the @*INC');
is(@*INC[0], '../lib/My/Module', '... and it is what and where we expected it to be');

is(+@ORIG_INC, $inc_count, 'our ORIG_INC is the same count');