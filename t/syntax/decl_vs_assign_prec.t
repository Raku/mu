#!/usr/bin/pugs

use v6;
use Test;

plan 6;

is(try{my $t; $t = (1 == 1) ?? "true" :: "false"; $t}, "true", 'my $t; $t = (cond) ?? :: gets value from ?? ::, not conds bool');
is(try{my $t; $t = (1 == 0) ?? "true" :: "false"; $t}, "false", '.. also for false');
is(try{our $t; $t = (1 == 1) ?? "true" :: "false"; $t}, "true", 'truth with "our"');
is(try{our $t; $t = (1 == 0) ?? "true" :: "false"; $t}, "false", '... and false');
is(try{my $t = (1 == 1) ?? "true" :: "false"; $t}, "true", 'my $t = (cond) ?? :: gets value from ?? ::');
is(try{my $t = (1 == 0) ?? "true" :: "false"; $t}, "false", '.. also for false');
