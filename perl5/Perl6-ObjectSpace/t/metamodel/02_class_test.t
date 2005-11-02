#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use_ok('Perl6::MetaModel::Bootstrap');

# some basic tests on the model so far ...

isa_ok($::Class, 'opaque');
is($::Class->id->equal_to(num->new(1)), $bit::TRUE, '... this is the first instance');
is($::Class->class, $::Class, 'Class is an instance of Class');

# now test some of the metamodel stuff ....

my $id = $::Class->send('id');
isa_ok($id, 'num');
is($id->equal_to($::Class->id), $bit::TRUE, '... id is just a thin wrapper around opaque::id');

dies_ok {
    $::Class->sent('id' => (num->new(100)));
} '... id only takes one parameter';

my $class = $::Class->send('class');
isa_ok($class, 'opaque');
is($class, $::Class->class, '... class is just a thin wrapper around opaque::class');

dies_ok {
    $::Class->sent('class' => (num->new(100)));
} '... class only takes one parameter';

my $superclasses = $::Class->send('superclasses');
isa_ok($superclasses, 'list');
is($superclasses->is_empty, $bit::TRUE, '... the superclass list is currently empty');

my $subclasses = $::Class->send('subclasses');
isa_ok($subclasses, 'list');
is($subclasses->is_empty, $bit::TRUE, '... the subclasses list is currently empty');
