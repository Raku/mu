#ifndef SMOP_BUILTIN_H
#define SMOP_BUILTIN_H

/*
 * The builtin types have their prototypes declared here so when using
 * directly them in low-level no name resolution is needed, even
 * because eventually these objects are needed to implement the name
 * resolution.
 */

#include <smop_base.h> // this is declared by smop.h which is the
                        // one who includes this file, but let's keep
                        // this here if anyone wants to include only
                        // parts of smop.h. It should do no harm
                        // because of the ifndefs of the beggining of
                        // the file.

/*
 * The undefined types
 */
extern SMOP__Object* SMOP__UNDEF__Whatever;
extern SMOP__Object* SMOP__UNDEF__Failure;

/* 
 * Immutable types
 */
extern SMOP__Object* SMOP__IMMUT__Bit; //        Perl single bit (allows traits, aliasing, undef, etc.)
extern SMOP__Object* SMOP__IMMUT__Int; //        Perl integer (allows Inf/NaN, arbitrary precision, etc.)
extern SMOP__Object* SMOP__IMMUT__Str; //        Perl string (finite sequence of Unicode characters)
extern SMOP__Object* SMOP__IMMUT__Num; //        Perl number
extern SMOP__Object* SMOP__IMMUT__Complex; //    Perl complex number
extern SMOP__Object* SMOP__IMMUT__Bool; //       Perl boolean
extern SMOP__Object* SMOP__IMMUT__Exception; //  Perl exception
extern SMOP__Object* SMOP__IMMUT__Code; //       Base class for all executable objects
extern SMOP__Object* SMOP__IMMUT__Block; //      Executable objects that have lexical scopes
extern SMOP__Object* SMOP__IMMUT__List; //       Lazy Perl list (composed of immutables and iterators)
extern SMOP__Object* SMOP__IMMUT__Seq; //        Completely evaluated (hence immutable) sequence
extern SMOP__Object* SMOP__IMMUT__Range; //      A pair of Ordered endpoints; gens immutables when iterated
extern SMOP__Object* SMOP__IMMUT__Set; //        Unordered collection of values that allows no duplicates
extern SMOP__Object* SMOP__IMMUT__Bag; //        Unordered collection of values that allows duplicates
extern SMOP__Object* SMOP__IMMUT__Junction; //   Set with additional behaviors
extern SMOP__Object* SMOP__IMMUT__Pair; //       A single key-to-value association
extern SMOP__Object* SMOP__IMMUT__Mapping; //    Set of Pairs with no duplicate keys
extern SMOP__Object* SMOP__IMMUT__Signature; //  Function parameters (left-hand side of a binding)
extern SMOP__Object* SMOP__IMMUT__Capture; //    Function call arguments (right-hand side of a binding)
extern SMOP__Object* SMOP__IMMUT__Blob; //       An undifferentiated mass of bits

/*
 * Mutable types
 */
extern SMOP__Object* SMOP__MUTAB__Scalar; //      Perl scalar
extern SMOP__Object* SMOP__MUTAB__Array; //       Perl array
extern SMOP__Object* SMOP__MUTAB__Hash; //        Perl hash
extern SMOP__Object* SMOP__MUTAB__KeyHash; //     Perl hash that autodeletes values matching default
extern SMOP__Object* SMOP__MUTAB__KeySet; //      KeyHash of Bool (does Set in list/array context)
extern SMOP__Object* SMOP__MUTAB__KeyBag; //      KeyHash of UInt (does Bag in list/array context)
extern SMOP__Object* SMOP__MUTAB__Buf; //         Perl buffer (a stringish array of memory locations)
extern SMOP__Object* SMOP__MUTAB__IO; //          Perl filehandle
extern SMOP__Object* SMOP__MUTAB__Routine; //     Base class for all wrappable executable objects
extern SMOP__Object* SMOP__MUTAB__Sub; //         Perl subroutine
extern SMOP__Object* SMOP__MUTAB__Method; //      Perl method
extern SMOP__Object* SMOP__MUTAB__Submethod; //   Perl subroutine acting like a method
extern SMOP__Object* SMOP__MUTAB__Macro; //       Perl compile-time subroutine
extern SMOP__Object* SMOP__MUTAB__Regex; //       Perl pattern
extern SMOP__Object* SMOP__MUTAB__Match; //       Perl match, usually produced by applying a pattern
extern SMOP__Object* SMOP__MUTAB__Package; //     Perl 5 compatible namespace
extern SMOP__Object* SMOP__MUTAB__Module; //      Perl 6 standard namespace
extern SMOP__Object* SMOP__MUTAB__Class; //       Perl 6 standard class namespace
extern SMOP__Object* SMOP__MUTAB__Role; //        Perl 6 standard generic interface/implementation
extern SMOP__Object* SMOP__MUTAB__Grammar; //     Perl 6 pattern matching namespace
extern SMOP__Object* SMOP__MUTAB__Any; //         Perl 6 object (default parameter type, excludes Junction)
extern SMOP__Object* SMOP__MUTAB__Object; //      Perl 6 object (either Any or Junction)

#endif
