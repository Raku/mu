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
    $::Class->send('id' => (num->new(100)));
} '... id only takes one parameter';

my $class = $::Class->send('class');
isa_ok($class, 'opaque');
is($class, $::Class->class, '... class is just a thin wrapper around opaque::class');

dies_ok {
    $::Class->send('class' => (num->new(100)));
} '... class only takes one parameter';

is($::Class->send('is_a' => $::Class), $bit::TRUE, '... Class is_a Class');

my $superclasses = $::Class->send('superclasses');
isa_ok($superclasses, 'list');
is($superclasses->is_empty, $bit::FALSE, '... the superclass list is currently empty');
is($superclasses->length->equal_to(num->new(1)), $bit::TRUE, '... the superclass list has one item in it');
is($superclasses->fetch(num->new(0)), $::Object, '... and that item is Object');

my $subclasses = $::Class->send('subclasses');
isa_ok($subclasses, 'list');
is($subclasses->is_empty, $bit::TRUE, '... the subclasses list is currently empty');

my $MRO = $::Class->send('MRO');
isa_ok($MRO, 'list');
is($MRO->is_empty, $bit::FALSE, '... the MRO list is not empty');
is($MRO->length->equal_to(num->new(2)), $bit::TRUE, '... the MRO list has two items in it');
is($MRO->fetch(num->new(0)), $::Class, '... and that first item is Class');
is($MRO->fetch(num->new(1)), $::Object, '... and that second item is Object');

{
    my $Foo;
    lives_ok {
        $Foo = $::Class->send('new');
    } '... we created a new class successfully';

    isa_ok($Foo, 'opaque');
    is($Foo->id->equal_to(num->new(3)), $bit::TRUE, '... this is the second instance');
    is($Foo->class, $::Class, 'Class is an instance of Class');
    
    is($Foo->send('is_a' => $Foo), $bit::TRUE, '... Foo is_a Foo');    
    
    my $superclasses = $Foo->send('superclasses');
    isa_ok($superclasses, 'list');
    is($superclasses->is_empty, $bit::TRUE, '... the superclass list is currently empty');

    my $subclasses = $Foo->send('subclasses');
    isa_ok($subclasses, 'list');
    is($subclasses->is_empty, $bit::TRUE, '... the subclasses list is currently empty');

    my $MRO = $Foo->send('MRO');
    isa_ok($MRO, 'list');
    is($MRO->is_empty, $bit::FALSE, '... the MRO list is not empty');
    is($MRO->length->equal_to(num->new(1)), $bit::TRUE, '... the MRO list has one item in it');
    is($MRO->fetch(num->new(0)), $Foo, '... and that item is Foo');    
    
}


