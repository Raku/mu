#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 3;
use Test::Exception;

=pod

This test checks a number of different bad heirarchies.

=cut

use Perl6::MetaModel;

=pod

Somewhat convoluted multiple inheritance, with 
some repeated inheritance as well.

        ::Object
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

my $Foo = class 'Foo' => { is => [ $::Object ] };   
my $Bar = class 'Bar' => { is => [ $Foo      ] };
my $Baz = class 'Baz' => { is => [ $::Object ] };
throws_ok {
    class 'FooBar' => {
        is => [ $Foo, $Bar ]
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

my $Diamond_A = class 'Diamond_A' => { is => [ $::Object ] };
my $Diamond_B = class 'Diamond_B' => {
    is => [ $Diamond_A ]
};
my $Diamond_C = class 'Diamond_C' => {
    is => [ $Diamond_A ]
};
my $Diamond2_D = class 'Diamond2_D' => {
    is => [ $Diamond_B, $Diamond_C ]
};
my $Diamond2_E = class 'Diamond2_E' => {
    is => [ $Diamond_C, $Diamond_B ]
};
throws_ok {
    class 'Diamond2_F' => {
        is => [ $Diamond2_D, $Diamond2_E ]
    };
} qr/Inconsistent hierarchy/, '... this should die';


=pod

circular inheritence
    
  +-- B <-+
  |       |
  +-> A --+

=cut

my $B = class 'B' => { is => [ $::Object ] };
my $A = class 'A' => { is => [ $B ] };

throws_ok {
    $B->superclasses([ $::Object, $A ]);
} qr/Inconsistent hierarchy/, '... this should die';    

