#!/usr/bin/pugs

use v6;
require Test;

plan 6;

todo_is(eval 'my $t; $t = (1 == 1) ?? "true" :: "false"; $t', "true", 'my $t; $t = (cond) ?? :: gets value from ?? ::, not conds bool');
todo_is(eval 'my $t; $t = (1 == 0) ?? "true" :: "false"; $t', "false", '.. also for false');
todo_is(eval 'our $t; $t = (1 == 1) ?? "true" :: "false"; $t', "true", 'truth with "our"');
todo_is(eval 'our $t; $t = (1 == 0) ?? "true" :: "false"; $t', "false", '... and false');
is(eval 'my $t = (1 == 1) ?? "true" :: "false"; $t', "true", 'my $t = (cond) ?? :: gets value from ?? ::');
is(eval 'my $t = (1 == 0) ?? "true" :: "false"; $t', "false", '.. also for false');


