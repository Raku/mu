#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 11;
use Test::Exception;

use Perl6::MetaModel;

=pod

This is an example of Ruby-style singleton methods using the
Perl6::MetaModel. To the best of my knowledge, this is actually
how it is implemented in Ruby as well. 

Basically what happens is that for each instance of an class, an 
anonymous class is created. The anon-class then adds the class to
it's superclass list, and then the instance is created from the
anon class.

Here is a link to a description of how this works in Ruby:

http://www.rubygarden.org/ruby?ClassMethods/Discussion

=cut

my $ClassWithSingletonMethods = $::Class->new('$:name' => 'ClassWithSingletonMethods');
$ClassWithSingletonMethods->superclasses([ $::Class ]);
$ClassWithSingletonMethods->add_method('new' => ::make_method(sub {
    my ($class, %params) = @_;
    my $anon_class = $::Class->new('$:name' => 'AnonClass');
    $anon_class->superclasses([ $class ]);
    $anon_class->add_method('add_singleton_method' => ::make_method(sub {
        my ($self, $label, $method) = @_;
        ::opaque_instance_class($self)->add_method($label, $method);
    }));    
    $_[0] = $anon_class;  
    ::next_METHOD();
}));
isa_ok($ClassWithSingletonMethods, 'Class');

my $Foo = $ClassWithSingletonMethods->new('$:name' => 'Foo');
$Foo->superclasses([ $::Object ]);
isa_ok($Foo, 'ClassWithSingletonMethods');

my $foo = $Foo->new();
isa_ok($foo, 'Foo');

my $foo2 = $Foo->new();
isa_ok($foo2, 'Foo');

$foo->add_singleton_method('test' => ::make_method(sub { '$foo::test' }));

is($foo->test(), '$foo::test', '... the singleton method worked');

dies_ok {
    $foo2->test();
} '... the singleton method is only for $foo';

# test it again,...

my $Bar = class 'Bar' => {
    metaclass => $ClassWithSingletonMethods,
    is => [ $::Object ],
    methods => {
        'baz' => sub { 'Bar::baz' }
    }
};
isa_ok($Bar, 'ClassWithSingletonMethods');

my $bar = $Bar->new();
isa_ok($bar, 'Bar');

my $bar2 = $Bar->new();
isa_ok($bar2, 'Bar');

$bar->add_singleton_method('baz' => ::make_method(sub { '$bar::baz' }));

is($bar->baz, '$bar::baz', '... got the right singleton method, overriding the classes method');
is($bar2->baz, 'Bar::baz', '... but still got the right method from the classes');
