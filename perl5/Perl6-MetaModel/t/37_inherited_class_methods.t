#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 9;

use Perl6::MetaModel;

$::Object->add_singleton_method('object' => ::make_method(sub { 'Object::object' }));

my $Foo = $::Class->new('$:name' => 'Foo');
$Foo->superclasses([ $::Object ]);
$Foo->add_singleton_method('bar' => ::make_method(sub { 'Foo::bar' }));

is($Foo->object, 'Object::object', '... got our singleton method correctly from Object');
is($Foo->bar, 'Foo::bar', '... got our singleton method correctly');

my $Bar = $::Class->new('$:name' => 'Bar');
$Foo->superclasses([ $::Object ]);
$Bar->add_singleton_method('baz' => ::make_method(sub { 'Bar::baz' }));

is($Bar->object, 'Object::object', '... got our singleton method correctly from Object');
is($Bar->baz, 'Bar::baz', '... got our singleton method correctly');

# check that this works with single inheritence

my $Baz = $::Class->new('$:name' => 'Baz');
$Baz->superclasses([ $Foo ]);

is($Baz->object, 'Object::object', '... got our singleton method correctly from Object');
is($Baz->bar, 'Foo::bar', '... got our inherited singleton method correctly');

# check that this works with multiple inheritence

my $FooBar = $::Class->new('$:name' => 'FooBar');
$FooBar->superclasses([ $Foo, $Bar ]);

is($FooBar->object, 'Object::object', '... got our singleton method correctly from Object');
is($FooBar->bar, 'Foo::bar', '... got our mulitple inherited singleton method correctly');
is($FooBar->baz, 'Bar::baz', '... got our other mulitple inherited singleton method correctly');


