#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 18;
use Test::Exception; 

use Perl6::MetaModel;

my $CountedClass = $::Class->new('$:name' => 'CounterClass');
isa_ok($CountedClass, 'CounterClass');

$CountedClass->superclasses([ $::Class ]);

isa_ok($CountedClass, 'Class');
isa_ok($CountedClass, 'Object');

$CountedClass->add_attribute('$:count', ::make_attribute('$:count'));
ok($CountedClass->has_attribute('$:count'), '... the attribute was added successfully');

$CountedClass->add_method('new' => ::make_method(sub {
    ::opaque_instance_attrs($::SELF)->{'$:count'}++;
    return ::next_METHOD();
}, $CountedClass));

ok($CountedClass->has_method('new'), '... the new method was overridden successfully');

$CountedClass->add_method('count' => ::make_method(sub {
    ::opaque_instance_attrs($::SELF)->{'$:count'};
}, $CountedClass));

ok($CountedClass->has_method('count'), '... the count method was added successfully');

## create some CountedClass classes

my $Foo = $CountedClass->new('$:name' => 'Foo');
isa_ok($Foo, 'Foo');

can_ok($Foo, 'count');
is($Foo->count(), undef, '... we have no Foo instances');

$Foo->superclasses([ $::Object ]);
$Foo->add_method('bar' => ::make_method(sub { 'Foo::bar' }, $Foo));

ok($Foo->has_method('bar'), '... Foo is a proper Class, and can get methods added');

my $iFoo = $Foo->new();
isa_ok($iFoo, 'Foo');

is($Foo->count(), 1, '... we have one Foo instance');

can_ok($iFoo, 'bar');
is($iFoo->bar(), 'Foo::bar', '... got the right value from bar()');

foreach my $count (2 .. 5) {
    $Foo->new();
    is($Foo->count(), $count, '... we have ' . $count . ' Foo instances');    
}

