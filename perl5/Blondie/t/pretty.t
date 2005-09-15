#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';


my $m; BEGIN { use_ok($m = "Blondie::Emitter::Pretty") }

isa_ok(my $o = $m->new, $m);

isa_ok($o, "Blondie::Reducer");

can_ok($m, "string");


