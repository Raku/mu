#!/usr/bin/pugs

use v6;
require Test;

plan 10;

use_ok('lib');

my $inc_count = +@*INC;

import('lib': '../lib');

is($inc_count + 1, +@*INC, 'we have added one path to the @*INC');
is(@*INC[0], '../lib', '... and it is what and where we expected it to be');

# now check adding duplicates

import('lib': '../lib/My/Module', '../lib/My/Module');

is($inc_count + 2, +@*INC, 'we have added only one new path to the @*INC');
is(@*INC[0], '../lib/My/Module', '... and it is what and where we expected it to be');

# now check slurpy arguments are working correctly

my @paths = ('/path/to/dir', 'more/paths');
my @paths2 = ('/path/to/another/dir', 'more/paths/again');

'lib'.import(@paths, @paths2);

is($inc_count + 6, +@*INC, 'we have added four new paths to the @*INC');
is(@*INC[0], '/path/to/dir', '... and it is what and where we expected it to be');
is(@*INC[1], 'more/paths', '... and it is what and where we expected it to be');
is(@*INC[2], '/path/to/another/dir', '... and it is what and where we expected it to be');
is(@*INC[3], 'more/paths/again', '... and it is what and where we expected it to be');
