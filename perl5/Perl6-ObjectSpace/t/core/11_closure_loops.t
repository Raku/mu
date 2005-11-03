#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use_ok('Perl6::Core::Block');

my $e = closure::env->new();
isa_ok($e, 'closure::env');

$e->create('$i' => num->new(0));

my $condition = block->new($e, sub { (shift)->get('$i')->less_than(num->new(10)) });
isa_ok($condition, 'block');

my $block = block->new($e, sub {
                my $e = shift;
                $e->set('$i' => $e->get('$i')->increment);
            });
isa_ok($block, 'block');

$block->do_while($condition);

is($e->get('$i')->to_native, 10, '... value was incremented properly');