#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 45;

=pod

This test checks the :preorder option on the dispatcher
this is currently the default as well as how Perl 5 worked.

This test checks a number of different heirarchies.

NOTE:
The dispatcher will visit all the classes during it's traversal
it does not skip repeats since it needs to be able to be used
by 'next METHOD' and the like.

=cut

use Perl6::MetaModel;
use Perl6::Object;

=pod

Single inheritance.

   Perl6::Object
         ^
         |
       Shape
         ^
         |   
      Polygon
         ^
         |   
     Rectangle
         ^
         |
      Square

=cut

class Shape => {
    is => [ 'Perl6::Object' ],    
};
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
    my $d = ::dispatch(::meta('Square'), 'dispatcher', (':preorder'));
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
        is(::dispatch($metaclass, 'name'), shift(@control), '... got the metaclass we expected');
        $metaclass = $d->next();  
    }
}

=pod

Class diamond inheritance

    A
   / \
  B   C
   \ /
    D
    
=cut

class Diamond_A => {
    is => [ 'Perl6::Object' ],    
};
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
    my $d = ::dispatch(::meta('Diamond_D'), 'dispatcher', (':preorder'));
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
        is(::dispatch($metaclass, 'name'), shift(@control), '... got the metaclass we expected');
        $metaclass = $d->next();  
    }
}

=pod

Expanded diamond inheritance

    A
   / \
  B   C
  |   |
  D   E
   \ /
    F

=cut

class Diamond2_D => {
    is => [ 'Diamond_B' ]
};
class Diamond2_E => {
    is => [ 'Diamond_C' ]
};
class Diamond2_F => {
    is => [ 'Diamond2_D', 'Diamond2_E' ]
};

{    
    my $d = ::dispatch(::meta('Diamond2_F'), 'dispatcher', (':preorder'));
    isa_ok($d, 'Perl6::MetaClass::Dispatcher');    

    my @control = qw(
        Diamond2_F
            Diamond2_D
                Diamond_B
                    Diamond_A
                        Perl6::Object
            Diamond2_E
                Diamond_C
                    Diamond_A
                        Perl6::Object                    
    );

    my $metaclass = $d->next();
    while (defined $metaclass) {
        isa_ok($metaclass, 'Perl6::MetaClass');
        is(::dispatch($metaclass, 'name'), shift(@control), '... got the metaclass we expected');
        $metaclass = $d->next();  
    }
}
