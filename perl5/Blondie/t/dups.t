#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';


my $m; BEGIN { use_ok($m = "Blondie::Reducer::DuplicateFinder") }

isa_ok(my $d = $m->new, $m);

isa_ok($d, "Blondie::Reducer");

can_ok($m, "duplicate_nodes");

