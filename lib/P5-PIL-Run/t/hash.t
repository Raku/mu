#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';

my $m; BEGIN { use_ok($m = "P5::PIL::Run::Container::Hash") }

isa_ok(my $h = $m->new_instance, $m);


