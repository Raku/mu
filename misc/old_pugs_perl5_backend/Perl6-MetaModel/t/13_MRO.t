#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 13;
use Test::Exception;

use Perl6::MetaModel;

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

my $F = class 'F' => { is => [ $::Object ] };
my $E = class 'E' => { is => [ $::Object ] };
my $D = class 'D' => { is => [ $::Object ] };
my $C = class 'C' => { is => [ $D, $F ] };
my $B = class 'B' => { is => [ $D, $E ] };
my $A = class 'A' => { is => [ $B, $C ] };

is_deeply(
    [ $F->MRO ],
    [ $F, $::Object ],
    '... got the right MRO for F');

is_deeply(
    [ $E->MRO ],
    [ $E, $::Object ],
    '... got the right MRO for E');    

is_deeply(
    [ $D->MRO ],
    [ $D, $::Object ],
    '... got the right MRO for D');       

is_deeply(
    [ $C->MRO ],
    [ $C, $D, $F, $::Object ],
    '... got the right MRO for C'); 

is_deeply(
    [ $B->MRO ],
    [ $B, $D, $E, $::Object ],
    '... got the right MRO for B');     

is_deeply(
    [ $A->MRO ],
    [ $A, $B, $C, $D, $E, $F, $::Object ],
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

my $X = class 'X' => { is => [ $::Object ] };
my $Y = class 'Y' => { is => [ $::Object ] };
my $XY = class 'XY' => { is => [ $X, $Y ] };
my $YX = class 'YX' => { is => [ $Y, $X ] };

throws_ok {
    class 'Z' => { is => [ $XY, $YX ] };
} qr/Inconsistent hierarchy/, '... got the right error with an inconsistent hierarchy';

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

my $F2 = class 'F2' => { is => [ $::Object ] };
my $E2 = class 'E2' => { is => [ $::Object ] };
my $D2 = class 'D2' => { is => [ $::Object ] };
my $C2 = class 'C2' => { is => [ $D2, $F2 ] };
my $B2 = class 'B2' => { is => [ $E2, $D2 ] };
my $A2 = class 'A2' => { is => [ $B2, $C2 ] };


is_deeply(
    [ $A2->MRO ],
    [ $A2, $B2, $E2, $C2, $D2, $F2, $::Object ],
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

my $Diamond_C = class 'Diamond_C' => { is => [ $::Object ] };
my $Diamond_A = class 'Diamond_A' => { is => [ $Diamond_C ] };
my $Diamond_B = class 'Diamond_B' => { is => [ $Diamond_C ] };
my $Diamond_D = class 'Diamond_D' => { is => [ $Diamond_A, $Diamond_B ] };

is_deeply(
    [ $Diamond_D->MRO ],
    [ $Diamond_D, $Diamond_A, $Diamond_B, $Diamond_C, $::Object ],
    '... got the right MRO for diamond inheritance');      

=pod

      $::Object
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

my $LifeForm    = class 'LifeForm'    => { is => [ $::Object ] };
my $Sentient    = class 'Sentient'    => { is => [ $LifeForm ] };
my $BiPedal     = class 'BiPedal'     => { is => [ $LifeForm ] };
my $Intelligent = class 'Intelligent' => { is => [ $Sentient ] };
my $Humanoid    = class 'Humanoid'    => { is => [ $BiPedal  ] };
my $Vulcan = class 'Vulcan' => {
    is => [ $Intelligent, $Humanoid ]
};

is_deeply(
    [ $Vulcan->MRO ],
    [ $Vulcan, $Intelligent, $Sentient, $Humanoid, $BiPedal, $LifeForm, $::Object ],
    '... got the right list for the Vulcan Dylan Example');  

=pod

More Dylan examples

from L<http://www.webcom.com/haahr/dylan/linearization-oopsla96.html>

=cut

my $Boat = class 'Boat' => { is => [ $::Object ] };

my $DayBoat = class 'DayBoat' => { is => [ $Boat ] };
my $WheelBoat = class 'WheelBoat' => { is => [ $Boat ] };

my $EngineLess = class 'EngineLess' => { is => [ $DayBoat ] };
my $SmallMultiHull = class 'SmallMultiHull' => { is => [ $DayBoat ] };

my $PedalWheelBoat = class 'PedalWheelBoat' => { is => [ $EngineLess, $WheelBoat ] };

my $SmallCatamaran = class 'SmallCatamaran' => { is => [ $SmallMultiHull ] };

my $Pedalo = class 'Pedalo' => { is => [ $PedalWheelBoat, $SmallCatamaran ] };

is_deeply(
    [ $PedalWheelBoat->MRO ],
    [ $PedalWheelBoat, $EngineLess, $DayBoat, $WheelBoat, $Boat, $::Object ],
    '... got the right list for PedalWheelBoat in the Pedalo Dylan Example');  

is_deeply(
    [ $SmallCatamaran->MRO ],
    [ $SmallCatamaran, $SmallMultiHull, $DayBoat, $Boat, $::Object ],
    '... got the right list for SmallCatamaran in the Pedalo Dylan Example');  

is_deeply(
    [ $Pedalo->MRO ],
    [ $Pedalo, $PedalWheelBoat, $EngineLess, $SmallCatamaran, $SmallMultiHull, $DayBoat, $WheelBoat, $Boat, $::Object ],
    '... got the right list for the Pedalo Dylan Example');  
