#ifndef YAP6_BUILTIN_H
#define YAP6_BUILTIN_H

/*
 * The builtin types have their prototypes declared here so when using
 * directly them in low-level no name resolution is needed, even
 * because eventually these objects are needed to implement the name
 * resolution.
 */

#include <yap6_base.h> // this is declared by yap6.h which is the one
                       // who includes this file, but let's keep this
                       // here if anyone wants to include only parts
                       // of yap6.h. It should do no harm because of
                       // the ifndefs of the beggining of the file.

/*
 * The undefined types
 */
extern YAP6__Prototype* YAP6__UNDEF__Undef;
extern YAP6__Prototype* YAP6__UNDEF__Whatever;
extern YAP6__Prototype* YAP6__UNDEF__Failure;

/* 
 * Immutable types
 */
extern YAP6__Prototype* YAP6__IMMUT__Bit; //        Perl single bit (allows traits, aliasing, undef, etc.)
extern YAP6__Prototype* YAP6__IMMUT__Int; //        Perl integer (allows Inf/NaN, arbitrary precision, etc.)
extern YAP6__Prototype* YAP6__IMMUT__Str; //        Perl string (finite sequence of Unicode characters)
extern YAP6__Prototype* YAP6__IMMUT__Num; //        Perl number
extern YAP6__Prototype* YAP6__IMMUT__Complex; //    Perl complex number
extern YAP6__Prototype* YAP6__IMMUT__Bool; //       Perl boolean
extern YAP6__Prototype* YAP6__IMMUT__Exception; //  Perl exception
extern YAP6__Prototype* YAP6__IMMUT__Code; //       Base class for all executable objects
extern YAP6__Prototype* YAP6__IMMUT__Block; //      Executable objects that have lexical scopes
extern YAP6__Prototype* YAP6__IMMUT__List; //       Lazy Perl list (composed of immutables and iterators)
extern YAP6__Prototype* YAP6__IMMUT__Seq; //        Completely evaluated (hence immutable) sequence
extern YAP6__Prototype* YAP6__IMMUT__Range; //      A pair of Ordered endpoints; gens immutables when iterated
extern YAP6__Prototype* YAP6__IMMUT__Set; //        Unordered collection of values that allows no duplicates
extern YAP6__Prototype* YAP6__IMMUT__Bag; //        Unordered collection of values that allows duplicates
extern YAP6__Prototype* YAP6__IMMUT__Junction; //   Set with additional behaviors
extern YAP6__Prototype* YAP6__IMMUT__Pair; //       A single key-to-value association
extern YAP6__Prototype* YAP6__IMMUT__Mapping; //    Set of Pairs with no duplicate keys
extern YAP6__Prototype* YAP6__IMMUT__Signature; //  Function parameters (left-hand side of a binding)
extern YAP6__Prototype* YAP6__IMMUT__Capture; //    Function call arguments (right-hand side of a binding)
extern YAP6__Prototype* YAP6__IMMUT__Blob; //       An undifferentiated mass of bits

/*
 * Mutable types
 */
extern YAP6__Prototype* YAP6__MUTAB__Scalar; //      Perl scalar
extern YAP6__Prototype* YAP6__MUTAB__Array; //       Perl array
extern YAP6__Prototype* YAP6__MUTAB__Hash; //        Perl hash
extern YAP6__Prototype* YAP6__MUTAB__KeyHash; //     Perl hash that autodeletes values matching default
extern YAP6__Prototype* YAP6__MUTAB__KeySet; //      KeyHash of Bool (does Set in list/array context)
extern YAP6__Prototype* YAP6__MUTAB__KeyBag; //      KeyHash of UInt (does Bag in list/array context)
extern YAP6__Prototype* YAP6__MUTAB__Buf; //         Perl buffer (a stringish array of memory locations)
extern YAP6__Prototype* YAP6__MUTAB__IO; //          Perl filehandle
extern YAP6__Prototype* YAP6__MUTAB__Routine; //     Base class for all wrappable executable objects
extern YAP6__Prototype* YAP6__MUTAB__Sub; //         Perl subroutine
extern YAP6__Prototype* YAP6__MUTAB__Method; //      Perl method
extern YAP6__Prototype* YAP6__MUTAB__Submethod; //   Perl subroutine acting like a method
extern YAP6__Prototype* YAP6__MUTAB__Macro; //       Perl compile-time subroutine
extern YAP6__Prototype* YAP6__MUTAB__Regex; //       Perl pattern
extern YAP6__Prototype* YAP6__MUTAB__Match; //       Perl match, usually produced by applying a pattern
extern YAP6__Prototype* YAP6__MUTAB__Package; //     Perl 5 compatible namespace
extern YAP6__Prototype* YAP6__MUTAB__Module; //      Perl 6 standard namespace
extern YAP6__Prototype* YAP6__MUTAB__Class; //       Perl 6 standard class namespace
extern YAP6__Prototype* YAP6__MUTAB__Role; //        Perl 6 standard generic interface/implementation
extern YAP6__Prototype* YAP6__MUTAB__Grammar; //     Perl 6 pattern matching namespace
extern YAP6__Prototype* YAP6__MUTAB__Any; //         Perl 6 object (default parameter type, excludes Junction)
extern YAP6__Prototype* YAP6__MUTAB__Object; //      Perl 6 object (either Any or Junction)

#endif
