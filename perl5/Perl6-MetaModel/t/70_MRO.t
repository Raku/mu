#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 13;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::Object;

=pod

"My first example"
class O: pass
class F(O): pass
class E(O): pass
class D(O): pass
class C(D,F): pass
class B(D,E): pass
class A(B,C): pass

                          6
                         ---
Level 3                 | O |                  (more general)
                      /  ---  \
                     /    |    \                      |
                    /     |     \                     |
                   /      |      \                    |
                  ---    ---    ---                   |
Level 2        3 | D | 4| E |  | F | 5                |
                  ---    ---    ---                   |
                   \  \ _ /       |                   |
                    \    / \ _    |                   |
                     \  /      \  |                   |
                      ---      ---                    |
Level 1            1 | B |    | C | 2                 |
                      ---      ---                    |
                        \      /                      |
                         \    /                      \ /
                           ---
Level 0                 0 | A |                (more specialized)
                           ---

=cut

class F => { is => [ 'Perl6::Object' ] };
class E => { is => [ 'Perl6::Object' ] };
class D => { is => [ 'Perl6::Object' ] };
class C => { is => [ 'D', 'F' ] };
class B => { is => [ 'D', 'E' ] };
class A => { is => [ 'B', 'C' ] };

is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(F->meta, 'MRO') ],
    [ qw(F Perl6::Object) ],
    '... got the right MRO for F');
    
is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(E->meta, 'MRO') ],
    [ qw(E Perl6::Object) ],
    '... got the right MRO for E');    
    
is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(D->meta, 'MRO') ],
    [ qw(D Perl6::Object) ],
    '... got the right MRO for D');       

is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(C->meta, 'MRO') ],
    [ qw(C D F Perl6::Object) ],
    '... got the right MRO for C'); 
    
is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(B->meta, 'MRO') ],
    [ qw(B D E Perl6::Object) ],
    '... got the right MRO for B');     
    
is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(A->meta, 'MRO') ],
    [ qw(A B C D E F Perl6::Object) ],
    '... got the right MRO for A');      
    
=pod

"Serious order disagreement" #From Guido
class O: pass
class X(O): pass
class Y(O): pass
class A(X,Y): pass
class B(Y,X): pass
try:
    class Z(A,B): pass #creates Z(A,B) in Python 2.2
except TypeError:
    pass # Z(A,B) cannot be created in Python 2.3

=cut

class X => { is => [ 'Perl6::Object' ] };
class Y => { is => [ 'Perl6::Object' ] };
class XY => { is => [ 'X', 'Y' ] };
class YX => { is => [ 'Y', 'X' ] };

throws_ok {
    class Z  => { is => [ 'XY', 'YX' ] };
} qr/^Inconsistent hierarchy/, '... got the right error with an inconsistent hierarchy';

=pod

"My second example"
class O: pass
class F(O): pass
class E(O): pass
class D(O): pass
class C(D,F): pass
class B(E,D): pass
class A(B,C): pass

                           6
                          ---
Level 3                  | O |
                       /  ---  \
                      /    |    \
                     /     |     \
                    /      |      \
                  ---     ---    ---
Level 2        2 | E | 4 | D |  | F | 5
                  ---     ---    ---
                   \      / \     /
                    \    /   \   /
                     \  /     \ /
                      ---     ---
Level 1            1 | B |   | C | 3
                      ---     ---
                       \       /
                        \     /
                          ---
Level 0                0 | A |
                          ---

>>> A.mro()
(<class '__main__.A'>, <class '__main__.B'>, <class '__main__.E'>,
<class '__main__.C'>, <class '__main__.D'>, <class '__main__.F'>,
<type 'object'>)

=cut
    
class F2 => { is => [ 'Perl6::Object' ] };
class E2 => { is => [ 'Perl6::Object' ] };
class D2 => { is => [ 'Perl6::Object' ] };
class C2 => { is => [ 'D2', 'F2' ] };
class B2 => { is => [ 'E2', 'D2' ] };
class A2 => { is => [ 'B2', 'C2' ] };


is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(A2->meta, 'MRO') ],
    [ qw(A2 B2 E2 C2 D2 F2 Perl6::Object) ],
    '... got the right MRO for A2');      
    
=pod

   C
  / \
 /   \
A     B
 \   /
  \ /
   D

=cut

class Diamond_C => { is => [ 'Perl6::Object' ] };
class Diamond_A => { is => [ 'Diamond_C' ] };
class Diamond_B => { is => [ 'Diamond_C' ] };
class Diamond_D => { is => [ 'Diamond_A', 'Diamond_B' ] };

is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(Diamond_D->meta, 'MRO') ],
    [ qw(Diamond_D Diamond_A Diamond_B Diamond_C Perl6::Object) ],
    '... got the right MRO for diamond inheritance');      

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

is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(Vulcan->meta, 'MRO') ],
    [ qw(Vulcan Intelligent Sentient Humanoid BiPedal LifeForm Perl6::Object) ],
    '... got the right list for the Vulcan Dylan Example');  
    
=pod

More Dylan examples

from L<http://www.webcom.com/haahr/dylan/linearization-oopsla96.html>

=cut

class Boat => { is => [ 'Perl6::Object' ] };

class DayBoat   => { is => [ 'Boat' ] };
class WheelBoat => { is => [ 'Boat' ] };

class EngineLess     => { is => [ 'DayBoat' ] };
class SmallMultiHull => { is => [ 'DayBoat' ] };

class PedalWheelBoat => { is => [ 'EngineLess', 'WheelBoat' ] };

class SmallCatamaran => { is => [ 'SmallMultiHull' ] };

class Pedalo => { is => [ 'PedalWheelBoat', 'SmallCatamaran' ] };

is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(PedalWheelBoat->meta, 'MRO') ],
    [ qw(PedalWheelBoat EngineLess DayBoat WheelBoat Boat Perl6::Object) ],
    '... got the right list for PedalWheelBoat in the Pedalo Dylan Example');  

is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(SmallCatamaran->meta, 'MRO') ],
    [ qw(SmallCatamaran SmallMultiHull DayBoat Boat Perl6::Object) ],
    '... got the right list for SmallCatamaran in the Pedalo Dylan Example');  

is_deeply(
    [ map { ::dispatch($_, 'name') } ::dispatch(Pedalo->meta, 'MRO') ],
    [ qw(Pedalo PedalWheelBoat EngineLess SmallCatamaran SmallMultiHull DayBoat WheelBoat Boat Perl6::Object) ],
    '... got the right list for the Pedalo Dylan Example');  














