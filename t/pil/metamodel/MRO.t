#!./pugs

use v6;
use Test::PIL::Bootstrap;

check_pil();

# check some of the MROs for the base classes

pil_is_eq('^Class.MRO()`length()', '4', '... ^Class.MRO().length() == 4');
pil_is_eq('^Class.MRO()`fetch(0)`eq(^Class)',   'true', '... ^Class.MRO()[0].eq(Class)');
pil_is_eq('^Class.MRO()`fetch(1)`eq(^Module)',  'true', '... ^Class.MRO()[1].eq(Module)');
pil_is_eq('^Class.MRO()`fetch(2)`eq(^Package)', 'true', '... ^Class.MRO()[2].eq(Package)');
pil_is_eq('^Class.MRO()`fetch(3)`eq(^Object)',  'true', '... ^Class.MRO()[3].eq(Object)');

pil_is_eq('^Module.MRO()`length()', '3', '... ^Module.MRO().length() == 3');
pil_is_eq('^Module.MRO()`fetch(0)`eq(^Module)',  'true', '... ^Module.MRO()[0].eq(Module)');
pil_is_eq('^Module.MRO()`fetch(1)`eq(^Package)', 'true', '... ^Module.MRO()[1].eq(Package)');
pil_is_eq('^Module.MRO()`fetch(2)`eq(^Object)',  'true', '... ^Module.MRO()[2].eq(Object)');

pil_is_eq('^Package.MRO()`length()', '2', '... ^Package.MRO().length() == 2');
pil_is_eq('^Package.MRO()`fetch(0)`eq(^Package)', 'true', '... ^Package.MRO()[0].eq(Package)');
pil_is_eq('^Package.MRO()`fetch(1)`eq(^Object)',  'true', '... ^Package.MRO()[1].eq(Object)');

pil_is_eq('^Object.MRO()`length()', '1', '... ^Object.MRO().length() == 1');
pil_is_eq('^Object.MRO()`fetch(0)`eq(^Object)', 'true', '... ^Object.MRO()[0].eq(Object)');

pil_is_eq('^Role.MRO()`length()', '4', '... ^Role.MRO().length() == 4');
pil_is_eq('^Role.MRO()`fetch(0)`eq(^Role)',    'true', '... ^Role.MRO()[0].eq(Role)');
pil_is_eq('^Role.MRO()`fetch(1)`eq(^Module)',  'true', '... ^Role.MRO()[1].eq(Module)');
pil_is_eq('^Role.MRO()`fetch(2)`eq(^Package)', 'true', '... ^Role.MRO()[2].eq(Package)');
pil_is_eq('^Role.MRO()`fetch(3)`eq(^Object)',  'true', '... ^Role.MRO()[3].eq(Object)');

## the MRO tests from Python and Dylan

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

pil_is_eq(q:to/EXAMPLE/
^F := ^Class.new({ '$!name' => 'F', '@!superclasses' => [ ^Object ] });
^E := ^Class.new({ '$!name' => 'E', '@!superclasses' => [ ^Object ] });
^D := ^Class.new({ '$!name' => 'D', '@!superclasses' => [ ^Object ] });
^C := ^Class.new({ '$!name' => 'C', '@!superclasses' => [ ^D, ^F ] });
^B := ^Class.new({ '$!name' => 'B', '@!superclasses' => [ ^D, ^E ] });
^A := ^Class.new({ '$!name' => 'A', '@!superclasses' => [ ^B, ^C ] });
-> $c { $c.name() }`do_for(^A.MRO());
EXAMPLE,
'["A", "B", "C", "D", "E", "F", "Object"]',
'... got the right MRO');

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

=cut

pil_is_eq(q:to/EXAMPLE/
^F := ^Class.new({ '$!name' => 'F', '@!superclasses' => [ ^Object ] });
^E := ^Class.new({ '$!name' => 'E', '@!superclasses' => [ ^Object ] });
^D := ^Class.new({ '$!name' => 'D', '@!superclasses' => [ ^Object ] });
^C := ^Class.new({ '$!name' => 'C', '@!superclasses' => [ ^D, ^F ] });
^B := ^Class.new({ '$!name' => 'B', '@!superclasses' => [ ^E, ^D ] });
^A := ^Class.new({ '$!name' => 'A', '@!superclasses' => [ ^B, ^C ] });
-> $c { $c.name() }`do_for(^A.MRO());
EXAMPLE,
'["A", "B", "E", "C", "D", "F", "Object"]',
'... got the right MRO');

=pod

   D
  / \
 /   \
B     C
 \   /
  \ /
   A

=cut

pil_is_eq(q:to/EXAMPLE/
^D := ^Class.new({ '$!name' => 'D', '@!superclasses' => [ ^Object ] });
^C := ^Class.new({ '$!name' => 'C', '@!superclasses' => [ ^D ] });
^B := ^Class.new({ '$!name' => 'B', '@!superclasses' => [ ^D ] });
^A := ^Class.new({ '$!name' => 'A', '@!superclasses' => [ ^B, ^C ] });
-> $c { $c.name() }`do_for(^A.MRO());
EXAMPLE,
'["A", "B", "C", "D", "Object"]',
'... got the right MRO');

=pod

      $^Object
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

pil_is_eq(q:to/EXAMPLE/
^LifeForm    := ^Class.new({ '$!name' => 'LifeForm',    '@!superclasses' => [ ^Object ] });
^Sentient    := ^Class.new({ '$!name' => 'Sentient',    '@!superclasses' => [ ^LifeForm ] });
^BiPedal     := ^Class.new({ '$!name' => 'BiPedal',     '@!superclasses' => [ ^LifeForm ] });
^Intelligent := ^Class.new({ '$!name' => 'Intelligent', '@!superclasses' => [ ^Sentient ] });
^Humanoid    := ^Class.new({ '$!name' => 'Humanoid',    '@!superclasses' => [ ^BiPedal ] });
^Vulcan      := ^Class.new({ '$!name' => 'Vulcan',      '@!superclasses' => [ ^Intelligent, ^Humanoid ] });
-> $c { $c.name() }`do_for(^Vulcan.MRO());
EXAMPLE,
'["Vulcan", "Intelligent", "Sentient", "Humanoid", "BiPedal", "LifeForm", "Object"]',
'... got the right MRO');


