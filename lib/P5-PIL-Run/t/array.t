#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';

use P5::PIL::Run::Container::Scalar;

my $m; BEGIN { use_ok($m = "P5::PIL::Run::Container::Array") };

isa_ok(my $a = $m->new, $m);

is($a->array_fetchSize, 0, "array size is 0");

is_deeply([ $a->array_fetchKeys ], [], "no keys in array");

is_deeply([ $a->array_fetch ], [], "no elements in array");

$a->array_push([21]);

is($a->array_fetchSize, 1, "array size is 1");

is_deeply([ $a->array_fetchKeys ], [ 0 ], "only key is '0'");


my $s = P5::PIL::Run::Container::Scalar->new;
$s->scalar_store(42);

$a->array_storeElem(1, $s);

is($a->array_fetchSize, 2, "array size is 2");
is_deeply([ $a->array_fetchKeys ], [ 0, 1 ], "two keys");

is($a->array_fetchVal(0), 21, "first value");
is($a->array_fetchVal(1), 42, "second value");

$a->array_storeVal(3, 84);
is($a->array_fetchSize, 4, "array size is 4");
is($a->array_fetchElem(3)->scalar_fetch, 84, "scalar container returns value");
