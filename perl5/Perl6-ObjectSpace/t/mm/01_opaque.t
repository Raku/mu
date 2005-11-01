#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::MM::Opaque');

my $o = opaque->new(
            reference->new($opaque::NULL_OBJECT), 
            hash->new(
                attribute->new('$.one') => nil->new()
            )
        );
isa_ok($o, 'opaque');

isa_ok($o->id, 'num');
cmp_ok($o->id->to_native, '==', 1, '... got the correct object id');

is($o->id, $o->to_num, '... to_num returns the id');
is($o->to_bit, $bit::TRUE, '... to_bit converts to true');
isa_ok($o->to_str, 'str');

isa_ok($o->class, 'opaque');
is($o->class, $opaque::NULL_OBJECT, '... our class is an NULL OBJECT');

isa_ok($o->get_attr(attribute->new('$.one')), 'nil');

$o->set_attr(attribute->new('$.one'), num->new(1));

isa_ok($o->get_attr(attribute->new('$.one')), 'num');
cmp_ok($o->get_attr(attribute->new('$.one'))->to_native, '==', 1, '... got the right value stored');

$o->change_class($o);

isa_ok($o->class, 'opaque');
is($o->class, $o, '... our class is ourselves');

my $o2 = opaque->new(reference->new($o), hash->new());
isa_ok($o2, 'opaque');

is($o2->id, $o2->to_num, '... to_num returns the id');
is($o2->to_bit, $bit::TRUE, '... to_bit converts to true');
isa_ok($o2->to_str, 'str');

isa_ok($o2->id, 'num');
cmp_ok($o2->id->to_native, '==', 2, '... got the correct object id');

isa_ok($o2->class, 'opaque');
is($o2->class, $o, '... our class is o');
