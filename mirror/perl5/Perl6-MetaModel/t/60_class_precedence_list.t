#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 5;

=pod

This test checks the :preorder option on the class_precedence_list
this is currently the default as well as how Perl 5 worked.

NOTE:
The class_precedence_list creates a list of unique classes in the 
order in which they should be called. It throws out any repeats.

=cut


use Perl6::MetaModel;
use Perl6::Object;

=pod

      Perl6::Object
           ^
           |
        LifeForm 
         ^    ^
        /      \
   Sentient    BiPedal
      ^          ^
      |          |
 Intelligent  Humanoid
       ^        ^
        \      /
         Vulcan

example taken from: L<http://gauss.gwydiondylan.org/books/drm/drm_50.html>

 define class <sentient> (<life-form>) end class;
 define class <bipedal> (<life-form>) end class;
 define class <intelligent> (<sentient>) end class;
 define class <humanoid> (<bipedal>) end class;
 define class <vulcan> (<intelligent>, <humanoid>) end class;

=cut

class LifeForm    => { is => [ 'Perl6::Object' ] };
class Sentient    => { is => [ 'LifeForm' ] };
class BiPedal     => { is => [ 'LifeForm' ] };
class Intelligent => { is => [ 'Sentient' ] };
class Humanoid    => { is => [ 'BiPedal' ]  };
class Vulcan => {
    is => [ 'Intelligent', 'Humanoid' ]
};

# pre-order class precedence list
is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(::meta('Vulcan'), 'class_precedence_list', (':preorder')) ],
    [ qw(Vulcan Intelligent Sentient LifeForm Perl6::Object Humanoid BiPedal) ],
    '... got the right :preorder class precedence list');
    
# pre-order class precedence list
is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(::meta('Vulcan'), 'class_precedence_list', (':breadth')) ],
    [ qw(Vulcan Intelligent Humanoid Sentient BiPedal LifeForm Perl6::Object ) ],
    '... got the right :breadth class precedence list');    

# least-derived first, like construction order    
is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(::meta('Vulcan'), 'class_precedence_list', (':ascendant')) ],
    [ qw(Vulcan Intelligent Sentient Humanoid BiPedal LifeForm Perl6::Object) ],
    '... got the right descendant class precedence list');    

# most-derived first, like destruction order    
is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(::meta('Vulcan'), 'class_precedence_list', (':descendant')) ],
    [ qw(Perl6::Object LifeForm BiPedal Humanoid Sentient Intelligent Vulcan) ],
    '... got the right ascendant class precedence list');      

=pod 

From the parrot test t/pmc/object-meths.t

 A   B A   E
  \ /   \ /
   C     D
    \   /
     \ /
      F

=cut

class A => { is => [ 'Perl6::Object' ] };
class B => { is => [ 'Perl6::Object' ] };
class E => { is => [ 'Perl6::Object' ] };
class C => { is => [ 'A', 'B' ] };
class D => { is => [ 'A', 'E' ] };
class F => { is => [ 'C', 'D' ] };

is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(::meta('F'), 'class_precedence_list', (':ascendant')) ],
    [ qw(F C D A B E Perl6::Object) ],
    '... got the right ascendant class precedence list');  
    
