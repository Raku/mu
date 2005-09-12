#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 4;
use Test::Exception;

use Perl6::MetaModel;

=pod

Testing that $?PACKAGE is bound when it should be

=cut

{
    my $FooPackage = $::Package->new('$:name' => 'Foo::Package');

    $FooPackage->STORE('&bar' => sub { $::PACKAGE });
    is($FooPackage->FETCH('&bar')->(), $FooPackage, '... got the $?PACKAGE value right for Package sub');
}

{
    my $FooClass = $::Class->new('$:name' => 'Foo::Class');
    $FooClass->superclasses([ $::Object ]);

    $FooClass->add_method('bar' => ::make_class_method(sub { $::PACKAGE }));
    $FooClass->add_method('bar2' => ::make_method(sub { $::PACKAGE }));  
    $FooClass->add_method('bar3' => ::make_submethod(sub { $::PACKAGE }));        
    
    is($FooClass->class::bar(), $FooClass, '... got the $?PACKAGE value right for class method');
    is($FooClass->new()->bar2(), $FooClass, '... got the $?PACKAGE value right for instance method'); 
    is($FooClass->new()->bar3(), $FooClass, '... got the $?PACKAGE value right for submethod');        
}