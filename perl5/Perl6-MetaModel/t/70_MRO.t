#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use Perl6::MetaModel;

=pod

def merge(seqs):
    print '\n\nCPL[%s]=%s' % (seqs[0][0],seqs),
    res = []; i=0
    while 1:
      nonemptyseqs=[seq for seq in seqs if seq]
      if not nonemptyseqs: return res
      i+=1; print '\n',i,'round: candidates...',
      for seq in nonemptyseqs: # find merge candidates among seq heads
          cand = seq[0]; print ' ',cand,
          nothead=[s for s in nonemptyseqs if cand in s[1:]]
          if nothead: cand=None #reject candidate
          else: break
      if not cand: raise "Inconsistent hierarchy"
      res.append(cand)
      for seq in nonemptyseqs: # remove cand
          if seq[0] == cand: del seq[0]

def mro(C):
    "Compute the class precedence list (mro) according to C3"
    return merge([[C]]+map(mro,C.__bases__)+[list(C.__bases__)])

=cut

sub merge {
    my (@seqs) = @_;
    my @res; 
    my $i = 0;
    while (1) {
        my @nonemptyseqs = (map { (@{$_} ? $_ : ()) } @seqs); # remove all empty seqences
        return @res if not @nonemptyseqs; # return the list if we have no more no-empty sequences
        $i++;
        my $cand; # a canidate ..
        foreach my $seq (@nonemptyseqs) {
            $cand = $seq->[0]; # get the head of the list
            # XXX - this is instead of the python "in"
            my $nothead;            
            foreach my $sub_seq (@nonemptyseqs) {
                my %in_tail = (map { $_->name => 1 } @{$sub_seq}[ 1 .. $#{$sub_seq} ]);
                $nothead++ if exists $in_tail{$cand->name};      
            }
            if ($nothead) {
                $cand = undef; # reject it ...
            }
            else {
                last;
            }
        }
        die "Inconsistent hierarchy" if not $cand;
        push @res => $cand;
        foreach my $seq (@nonemptyseqs) {
            if ($seq->[0]->name eq $cand->name) {
                shift @{$seq};
            }
        }
    }
}

use Scalar::Util 'blessed';

sub MRO {
    my ($c) = @_;
    return merge(
            [ $c ],                                    # the class we are linearizing
            (map { [ MRO($_) ] } @{$c->superclasses}), # the MRO of all the superclasses
            [ @{$c->superclasses} ]                    # a list of all the superclasses
            );
}

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

class F => {};
class E => {};
class D => {};
class C => { is => [ 'D', 'F' ] };
class B => { is => [ 'D', 'E' ] };
class A => { is => [ 'B', 'C' ] };

is_deeply(
    [ map { $_->name } MRO(F->meta) ],
    [ qw(F Perl6::Object) ],
    '... got the right MRO for F');
    
is_deeply(
    [ map { $_->name } MRO(E->meta) ],
    [ qw(E Perl6::Object) ],
    '... got the right MRO for E');    
    
is_deeply(
    [ map { $_->name } MRO(D->meta) ],
    [ qw(D Perl6::Object) ],
    '... got the right MRO for D');       

is_deeply(
    [ map { $_->name } MRO(C->meta) ],
    [ qw(C D F Perl6::Object) ],
    '... got the right MRO for C'); 
    
is_deeply(
    [ map { $_->name } MRO(B->meta) ],
    [ qw(B D E Perl6::Object) ],
    '... got the right MRO for B');     
    
is_deeply(
    [ map { $_->name } MRO(A->meta) ],
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

class X => {};
class Y => {};
class XY => { is => [ 'X', 'Y' ] };
class YX => { is => [ 'Y', 'X' ] };
class Z  => { is => [ 'XY', 'YX' ] };

throws_ok {
    MRO(Z->meta);
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
    
class F2 => {};
class E2 => {};
class D2 => {};
class C2 => { is => [ 'D2', 'F2' ] };
class B2 => { is => [ 'E2', 'D2' ] };
class A2 => { is => [ 'B2', 'C2' ] };


is_deeply(
    [ map { $_->name } MRO(A2->meta) ],
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

class Diamond_C => {};
class Diamond_A => { is => [ 'Diamond_C' ] };
class Diamond_B => { is => [ 'Diamond_C' ] };
class Diamond_D => { is => [ 'Diamond_A', 'Diamond_B' ] };

is_deeply(
    [ map { $_->name } MRO(Diamond_D->meta) ],
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

class LifeForm    => {};
class Sentient    => { is => [ 'LifeForm' ] };
class BiPedal     => { is => [ 'LifeForm' ] };
class Intelligent => { is => [ 'Sentient' ] };
class Humanoid    => { is => [ 'BiPedal' ]  };
class Vulcan => {
    is => [ 'Intelligent', 'Humanoid' ]
};

is_deeply(
    [ map { $_->name } MRO(Vulcan->meta) ],
    [ qw(Vulcan Intelligent Sentient Humanoid BiPedal LifeForm Perl6::Object) ],
    '... got the right list for the Vulcan Dylan Example');  
    
    

