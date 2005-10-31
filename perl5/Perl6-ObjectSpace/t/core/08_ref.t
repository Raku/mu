#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::Core::Ref');
use_ok('Perl6::Core::Num');

my $ref = reference->new(num->new(3));
isa_ok($ref, 'reference');

isa_ok($ref->fetch(), 'num');
cmp_ok($ref->fetch()->to_native, '==', 3, '... got the right value');

$ref->store($ref->fetch()->increment);

isa_ok($ref->fetch(), 'num');
cmp_ok($ref->fetch()->to_native, '==', 4, '... got the right value');

$ref->store(str->new('hello world'));

isa_ok($ref->fetch(), 'str');
is($ref->fetch()->to_native, 'hello world', '... got the right value');