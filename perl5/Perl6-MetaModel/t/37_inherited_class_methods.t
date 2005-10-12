#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 5;

use Perl6::MetaModel;

=pod

This is a proof of concept that the meta-model can actually support
a subclass of class which creates the proper eigenmethod structure
which will allow for class methods to be inherited.

The only restriction this makes is that the superclass list must
be passed into the class constructor, and not through the superclasses
method. It is likely with a little effort that this could be worked 
around actually.

=cut

my $ClassWithInheritedClassMethods = $::Class->new('$:name' => 'ClassWithInheritedClassMethods');
$ClassWithInheritedClassMethods->superclasses([ $::Class ]);

$ClassWithInheritedClassMethods->add_method('BUILD' => ::make_submethod(sub {
    my ($self, %params) = @_;
    if ($self->class->class != $::EigenClass) { 
        my $eigenclass = $::EigenClass->new('$:name' => 'EigenClass[' . $self->name . ']');
        my @eigen_superclasses = ( $self->class );
        @eigen_superclasses = map { 
            $_->class; 
        } @{$params{'@:superclasses'}} 
            if exists $params{'@:superclasses'};
        $eigenclass->superclasses([ @eigen_superclasses ]);
        ::opaque_instance_change_class($self, $eigenclass);
    }    
}));

my $Foo = $ClassWithInheritedClassMethods->new('$:name' => 'Foo', '@:superclasses' => [ $::Object ]);
$Foo->add_singleton_method('bar' => ::make_method(sub { 'Foo::bar' }));

is($Foo->bar, 'Foo::bar', '... got our singleton method correctly');

my $Bar = $ClassWithInheritedClassMethods->new('$:name' => 'Bar', '@:superclasses' => [ $::Object ]);
$Bar->add_singleton_method('baz' => ::make_method(sub { 'Bar::baz' }));

is($Bar->baz, 'Bar::baz', '... got our singleton method correctly');

# check that this works with single inheritence

my $Baz = $ClassWithInheritedClassMethods->new('$:name' => 'Baz', '@:superclasses' => [ $Foo ]);

is($Baz->bar, 'Foo::bar', '... got our inherited singleton method correctly');

# check that this works with multiple inheritence

my $FooBar = $ClassWithInheritedClassMethods->new('$:name' => 'FooBar', '@:superclasses' => [ $Foo, $Bar ]);

is($FooBar->bar, 'Foo::bar', '... got our mulitple inherited singleton method correctly');
is($FooBar->baz, 'Bar::baz', '... got our other mulitple inherited singleton method correctly');


