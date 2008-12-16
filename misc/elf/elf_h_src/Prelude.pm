
# Class graph

#     Bit         Perl single bit (allows traits, aliasing, undef, etc.)
class Bit is Any {};
#     Int         Perl integer (allows Inf/NaN, arbitrary precision, etc.)
class Int is Any {};
#     Str         Perl string (finite sequence of Unicode characters)
class Str is Any {};
#     Num         Perl number
class Num is Any {};
#     Complex     Perl complex number
class Complex is Any {};
#     Bool        Perl boolean
class Bool is Any {};
#     Exception   Perl exception
#     Code        Base class for all executable objects
class Code is Any {};
#     Block       Executable objects that have lexical scopes
class Block is Code {};
#     List        Lazy Perl list (composed of immutables and iterators)
class List is Any {};
#     Seq         Completely evaluated (hence immutable) sequence
class Seq is Any {};
#     Range       A pair of Ordered endpoints; gens immutables when iterated
class Range is Any {};
#     Set         Unordered collection of values that allows no duplicates
class Set is Any {};
#     Bag         Unordered collection of values that allows duplicates
class Bag is Any {};
#     Junction    Set with additional behaviors
class Junction is Object {};
#     Pair        A single key-to-value association
class Pair is Any {};
#     Mapping     Set of Pairs with no duplicate keys
class Mapping is Any {};
#     Signature   Function parameters (left-hand side of a binding)
class Signature is Any {};
#     Capture     Function call arguments (right-hand side of a binding)
class Capture is Any {};
#     Blob        An undifferentiated mass of bits
class Blob is Any {};

#     Scalar      Perl scalar
class Scalar is Any {};
#     Array       Perl array
class Array is List {};
#     Hash        Perl hash
class Hash is Any {};
#     KeyHash     Perl hash that autodeletes values matching default
class KeyHash is Any {};
#     KeySet      KeyHash of Bool (does Set in list/array context)
class KeySet is Any {};
#     KeyBag      KeyHash of UInt (does Bag in list/array context)
class KeyBag is Any {};
#     Buf         Perl buffer (a stringish array of memory locations)
class Buf is Any {};
#     IO          Perl filehandle
class IO is Any {};
#     Routine     Base class for all wrappable executable objects
class Routine is Code {};
#     Sub         Perl subroutine
class Sub is Routine {};
#     Method      Perl method
class Method is Routine {};
#     Submethod   Perl subroutine acting like a method
class Subethod is Routine {};
#     Macro       Perl compile-time subroutine
class Macro is Routine {};
#     Regex       Perl pattern
class Regex is Routine {};
#     Match       Perl match, usually produced by applying a pattern
class Match is Any {};
#     Package     Perl 5 compatible namespace
class Package is Any {};
#     Module      Perl 6 standard namespace
class Module is Package {};
#     Class       Perl 6 standard class namespace
class Class is Module {};
#     Role        Perl 6 standard generic interface/implementation
class Role is Module {};
#     Grammar     Perl 6 pattern matching namespace
class Grammar is Module {};
#     Any         Perl 6 object (default parameter type, excludes Junction)
class Any is Object {};
#     Object      Perl 6 object (either Any or Junction)
class Object {}; #XXX does Class 


class Pair {
  has $.key; has $.value;
}


class Any {
  method say() { say(self) }
}


package GLOBAL {

  sub say(*@a) {
    for @a { print $_.Str; }
    print "\n";
  }

  sub infix:<xx>(@a, Int $count){
    my @result; for 1 .. $count {@result.push(@a)}; @result;
  }

}

# .Num()
class Int   { method Num () { self } }
class Num   { method Num () { self } }
class Str   { method Num () { self.primitive_Num() } }
class Array { method Num () { self.elems } }
class Hash  { method Num () { self.keys.elems } }
class Pair  { method Num () { 2 } }; # so says pugs, the only impl working. 2008-May-24

# .Str()
class Int   { method Str () { ''~self } }
class Num   { method Str () { ''~self } }
class Str   { method Str () { self } }
class Array { method Str () { self.join('') } }
class Hash  { method Str () { self.keys.map(sub ($k){$k~"\t"~self{$k}}).join("\n") } }
class Pair  { method Str () { $.key~"\t"~$.value } }


class Any {
  method print() { say self }
}


class Any {
  method true() { defined(self) }
  method defined { defined(self) }
}

