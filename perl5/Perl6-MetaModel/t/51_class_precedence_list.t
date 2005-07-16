#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 1;

use Perl6::MetaModel;

=pod

example taken from:

L<http://gauss.gwydiondylan.org/books/drm/drm_50.html>

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
    [ map { $_->name } Vulcan->meta->class_precedence_list ],
    [ qw(Vulcan Intelligent Sentient LifeForm Perl6::Object Humanoid BiPedal) ],
    '... got the right preorder class precedence list');

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

    
    
    