#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use_ok('Perl6::Core::Bit');

# ON

my $on = bit->new(1);
isa_ok($on, 'bit');

my $on_str = $on->to_str;
isa_ok($on_str, 'str');
cmp_ok($on_str->to_native, 'eq', '1', '... got the right string');

my $on_num = $on->to_num;
isa_ok($on_num, 'num');
cmp_ok($on_num->to_native, '==', 1, '... got the right num');

cmp_ok($on->to_native, '==', 1, '... got the right native value');

is($on->to_bit, $on, '... on_bit returns itself');

# OFF

my $off = bit->new(0);
isa_ok($off, 'bit');

my $off_str = $off->to_str;
isa_ok($off_str, 'str');
cmp_ok($off_str->to_native, 'eq', '0', '... got the right string');

my $off_num = $off->to_num;
isa_ok($off_num, 'num');
cmp_ok($off_num->to_native, '==', 0, '... got the right num');

cmp_ok($off->to_native, '==', 0, '... got the right native value');

is($off->to_bit, $off, '... on_bit returns itself');

## errors

dies_ok {
    bit->new(5)
} '... cannot call constructor without 1 or 0';
