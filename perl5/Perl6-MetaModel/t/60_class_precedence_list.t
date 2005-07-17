#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 2;

=pod

This test checks the :preorder option on the class_precedence_list
this is currently the default as well as how Perl 5 worked.

NOTE:
The class_precedence_list creates a list of unique classes in the 
order in which they should be called. It throws out any repeats.

=cut


use Perl6::MetaModel;

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

class LifeForm    => {};
class Sentient    => { is => [ 'LifeForm' ] };
class BiPedal     => { is => [ 'LifeForm' ] };
class Intelligent => { is => [ 'Sentient' ] };
class Humanoid    => { is => [ 'BiPedal' ]  };
class Vulcan => {
    is => [ 'Intelligent', 'Humanoid' ]
};

# pre-order class precedence list
is_deeply(
    [ map { $_->name } Vulcan->meta->class_precedence_list(':preorder') ],
    [ qw(Vulcan Intelligent Sentient LifeForm Perl6::Object Humanoid BiPedal) ],
    '... got the right :preorder class precedence list');
    
# pre-order class precedence list
is_deeply(
    [ map { $_->name } Vulcan->meta->class_precedence_list(':breadth') ],
    [ qw(Vulcan Intelligent Humanoid Sentient BiPedal LifeForm Perl6::Object ) ],
    '... got the right :breadth class precedence list');    

=pod

# least-derived first, like construction order    
is_deeply(
    [ map { $_->name } Vulcan->meta->class_precedence_list(':descendant') ],
    [ qw(Vulcan Intelligent Sentient Humanoid BiPedal LifeForm Perl6::Object) ],
    '... got the right descendant class precedence list');    

# most-derived first, like destruction order    
is_deeply(
    [ map { $_->name } Vulcan->meta->class_precedence_list(':ascendant') ],
    [ qw(Perl6::Object LifeForm BiPedal Sentient Humanoid Intelligent Vulcan) ],
    '... got the right ascendant class precedence list');      

=cut

