#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';

use P5::PIL::Run::Container::Scalar;

my $m; BEGIN { use_ok($m = "P5::PIL::Run::Container::Array") };

isa_ok(my $a = $m->new_instance, $m);

is($a->array_fetchSize, 0, "array size is 0");

is_deeply([ $a->array_fetchKeys ], [], "no keys in array");

is_deeply([ $a->array_fetch ], [], "no elements in array");

$a->array_push([21]);

use Data::Dumper;
warn Dumper($a);

is($a->array_fetchSize, 1, "array size is 1");

is_deeply([ $a->array_fetchKeys ], [ 0 ], "only key is '0'");


my $s = P5::PIL::Run::Container::Scalar->new_instance;
$s->scalar_store(42);

