#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';

my $m; BEGIN { use_ok($m = "Blondie::Prelude") }

can_ok($m, "env");

isa_ok($m->env, "Blondie::Env");
