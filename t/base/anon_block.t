#!/usr/bin/pugs

use v6;
require Test;

plan 6;

my $anon_sub = sub { 1 };
is($anon_sub(), 1, 'sub { } works');

my $anon_sub_w_arg = sub ($arg) { 1 + $arg };
is($anon_sub_w_arg(3), 4, 'sub ($arg) {} works');

my $anon_block = { 1 };
is($anon_block(), 1, '{} <anon block> works');

my $pointy_block = -> { 1 };
is($pointy_block(), 1, '-> {} <"pointy" block> works');

my $pointy_block_w_arg = -> $arg { 1 + $arg };
is($pointy_block_w_arg(3), 4, '-> $arg {} <"pointy" block w/args> works');

my $pointy_block_w_multiple_args = -> $arg1, $arg2 { $arg1 + $arg2 };
is($pointy_block_w_multiple_args(3, 4), 7, '-> $arg1, $arg2 {} <"pointy" block w/multiple args> works');