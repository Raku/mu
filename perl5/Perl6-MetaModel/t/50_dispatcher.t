#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 45;

use Perl6::MetaModel;
    
class Foo => {};   
class Bar => {
    is => [ 'Foo' ]
};
class Baz => {};
class FooBar => {
    is => [ 'Foo', 'Bar' ]
};
class FooBarBaz => {
    is => [ 'FooBar', 'Baz' ]
};
    
{    
    my $d = FooBarBaz->meta->dispatcher();
    isa_ok($d, 'Perl6::MetaClass::Dispatcher');
    
    my @control = qw(
        FooBarBaz
            FooBar
                Foo
                    Perl6::Object
                Bar
                    Foo
                        Perl6::Object
            Baz
                Perl6::Object    
    );

    my $metaclass = $d->next();
    while (defined $metaclass) {
        isa_ok($metaclass, 'Perl6::MetaClass');        
        is($metaclass->name, shift(@control), '... got the metaclass we expected');
        $metaclass = $d->next();  
    }
}

class Shape => {};
class Polygon => {
    is => [ 'Shape' ]
};
class Rectangle => {
    is => [ 'Polygon' ]
};
class Square => {
    is => [ 'Rectangle' ]
};

{    
    my $d = Square->meta->dispatcher();
    isa_ok($d, 'Perl6::MetaClass::Dispatcher');    
    
    my @control = qw(
        Square
            Rectangle
                Polygon
                    Shape
                        Perl6::Object
    );

    my $metaclass = $d->next();
    while (defined $metaclass) {
        isa_ok($metaclass, 'Perl6::MetaClass');        
        is($metaclass->name, shift(@control), '... got the metaclass we expected');
        $metaclass = $d->next();  
    }
}

class Diamond_A => {};
class Diamond_B => {
    is => [ 'Diamond_A' ]
};
class Diamond_C => {
    is => [ 'Diamond_A' ]
};
class Diamond_D => {
    is => [ 'Diamond_B', 'Diamond_C' ]
};

{    
    my $d = Diamond_D->meta->dispatcher();
    isa_ok($d, 'Perl6::MetaClass::Dispatcher');    
    
    my @control = qw(
        Diamond_D
            Diamond_B
                Diamond_A
                    Perl6::Object
            Diamond_C
                Diamond_A
                    Perl6::Object                    
    );

    my $metaclass = $d->next();
    while (defined $metaclass) {
        isa_ok($metaclass, 'Perl6::MetaClass');
        is($metaclass->name, shift(@control), '... got the metaclass we expected');
        $metaclass = $d->next();  
    }
}


