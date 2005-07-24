#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 2;
use Test::Exception;

=pod

This test checks a number of different bad heirarchies.

=cut

use Perl6::MetaModel;
use Perl6::Object;

=pod

Somewhat convoluted multiple inheritance, with 
some repeated inheritance as well.

     Perl6::Object
         ^    ^    
        /     |
      Foo     |
      ^  ^    |
      |   \   |
      |  Bar  |  
      |   ^   |
      |  /    |
    FooBar   Baz
       \     /
      FooBarBaz

=cut

class Foo => { is => [ 'Perl6::Object' ] };   
class Bar => {
    is => [ 'Foo' ]
};
class Baz => { is => [ 'Perl6::Object' ] };
throws_ok {
    class FooBar => {
        is => [ 'Foo', 'Bar' ]
    };
} qr/Inconsistent hierarchy/, '... this should die';

=pod

Expanded diamond inheritance with repeated more repeats

   +---------+
   |     A   |
   |   /  \  |
   +--B    C |
      |  / | |
      | /  |/
      D    E
       \  /
        F

=cut

class Diamond_A => { is => [ 'Perl6::Object' ] };
class Diamond_B => {
    is => [ 'Diamond_A' ]
};
class Diamond_C => {
    is => [ 'Diamond_A' ]
};
class Diamond2_D => {
    is => [ 'Diamond_B', 'Diamond_C' ]
};
class Diamond2_E => {
    is => [ 'Diamond_C', 'Diamond_B' ]
};
throws_ok {
    class Diamond2_F => {
        is => [ 'Diamond2_D', 'Diamond2_E' ]
    };
} qr/Inconsistent hierarchy/, '... this should die';
