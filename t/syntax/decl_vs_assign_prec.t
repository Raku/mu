#!/usr/bin/pugs

use v6;
require Test;

plan 6;

is(eval 'my $t; $t = (1 == 1) ?? "true" :: "false"; $t', "true", 'my $t; $t = (cond) ?? :: gets value from ?? ::, not conds bool');
is(eval 'my $t; $t = (1 == 0) ?? "true" :: "false"; $t', "false", '.. also for false');
is(eval 'our $t; $t = (1 == 1) ?? "true" :: "false"; $t', "true", 'truth with "our"');
is(eval 'our $t; $t = (1 == 0) ?? "true" :: "false"; $t', "false", '... and false');
is(eval 'my $t = (1 == 1) ?? "true" :: "false"; $t', "true", 'my $t = (cond) ?? :: gets value from ?? ::');
is(eval 'my $t = (1 == 0) ?? "true" :: "false"; $t', "false", '.. also for false');


