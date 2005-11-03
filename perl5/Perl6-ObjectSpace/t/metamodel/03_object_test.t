#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use_ok('Perl6::MetaModel::Bootstrap');

isa_ok($::Object, 'opaque');
is($::Object->id->equal_to(num->new(2)), $bit::TRUE, '... this is the second instance');
is($::Object->class, $::Class, 'Object is an instance of Class');

is($::Object->send('is_a' => $::Object), $bit::TRUE, '... Object is_a Object');    

my $superclasses = $::Object->send('superclasses');
isa_ok($superclasses, 'list');
is($superclasses->is_empty, $bit::TRUE, '... the superclass list is currently empty');

my $MRO = $::Object->send('MRO');
isa_ok($MRO, 'list');
is($MRO->is_empty, $bit::FALSE, '... the MRO list is not empty');
is($MRO->length->equal_to(num->new(1)), $bit::TRUE, '... the MRO list has one item in it');
is($MRO->fetch(num->new(0)), $::Object, '... and that item is Object');

my $i = $::Object->send('new');
isa_ok($i, 'opaque');

is($i->id->equal_to(num->new(3)), $bit::TRUE, '... our id is the third id');
is($i->class, $::Object, '... our class is Object');

is($i->send('can' => symbol->new('BUILD'))->is_nil, $bit::FALSE, '... $i->can(BUILD)');
is($i->send('can' => symbol->new('BUILDALL'))->is_nil, $bit::FALSE, '... $i->can(BUILDALL)');
is($i->send('can' => symbol->new('can'))->is_nil, $bit::FALSE, '... $i->can(can)');
is($i->send('can' => symbol->new('id'))->is_nil, $bit::FALSE, '... $i->can(id)');
is($i->send('can' => symbol->new('class'))->is_nil, $bit::FALSE, '... $i->can(class)');

END {
    my $temp = $::Class;
}
