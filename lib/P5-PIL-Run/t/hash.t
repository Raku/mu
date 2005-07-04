#<style!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';

my $m; BEGIN { use_ok($m = "P5::PIL::Run::Container::Hash") }

isa_ok(my $h = $m->new, $m);

is_deeply([ $h->hash_fetchKeys ], [] , "no keys in hash yet");

ok($h->hash_isEmpty, "hash is empty");

$h->hash_storeVal("key" => "value");

ok(!$h->hash_isEmpty, "hash is not empty after insert");

is($h->hash_fetchVal("key"), "value", "retrieving stored key yields stored value");

is_deeply([ $h->hash_fetchKeys ], [ "key" ], "one key");

ok($h->hash_existsElem("key"), "exists");

$h->hash_clear;

ok($h->hash_isEmpty, "hash is empty after being cleaered");

