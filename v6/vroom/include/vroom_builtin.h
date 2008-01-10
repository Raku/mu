#ifndef VROOM_BUILTIN_H
#define VROOM_BUILTIN_H

/*
 * The builtin types have their prototypes declared here so when using
 * directly them in low-level no name resolution is needed, even
 * because eventually these objects are needed to implement the name
 * resolution.
 */

#include <vroom_base.h> // this is declared by vroom.h which is the
                        // one who includes this file, but let's keep
                        // this here if anyone wants to include only
                        // parts of vroom.h. It should do no harm
                        // because of the ifndefs of the beggining of
                        // the file.

/*
 * The undefined types
 */
extern VROOM__Object* VROOM__UNDEF__Undef;
extern VROOM__Object* VROOM__UNDEF__Whatever;
extern VROOM__Object* VROOM__UNDEF__Failure;

/* 
 * Immutable types
 */
extern VROOM__Object* VROOM__IMMUT__Bit; //        Perl single bit (allows traits, aliasing, undef, etc.)
extern VROOM__Object* VROOM__IMMUT__Int; //        Perl integer (allows Inf/NaN, arbitrary precision, etc.)
extern VROOM__Object* VROOM__IMMUT__Str; //        Perl string (finite sequence of Unicode characters)
extern VROOM__Object* VROOM__IMMUT__Num; //        Perl number
extern VROOM__Object* VROOM__IMMUT__Complex; //    Perl complex number
extern VROOM__Object* VROOM__IMMUT__Bool; //       Perl boolean
extern VROOM__Object* VROOM__IMMUT__Exception; //  Perl exception
extern VROOM__Object* VROOM__IMMUT__Code; //       Base class for all executable objects
extern VROOM__Object* VROOM__IMMUT__Block; //      Executable objects that have lexical scopes
extern VROOM__Object* VROOM__IMMUT__List; //       Lazy Perl list (composed of immutables and iterators)
extern VROOM__Object* VROOM__IMMUT__Seq; //        Completely evaluated (hence immutable) sequence
extern VROOM__Object* VROOM__IMMUT__Range; //      A pair of Ordered endpoints; gens immutables when iterated
extern VROOM__Object* VROOM__IMMUT__Set; //        Unordered collection of values that allows no duplicates
extern VROOM__Object* VROOM__IMMUT__Bag; //        Unordered collection of values that allows duplicates
extern VROOM__Object* VROOM__IMMUT__Junction; //   Set with additional behaviors
extern VROOM__Object* VROOM__IMMUT__Pair; //       A single key-to-value association
extern VROOM__Object* VROOM__IMMUT__Mapping; //    Set of Pairs with no duplicate keys
extern VROOM__Object* VROOM__IMMUT__Signature; //  Function parameters (left-hand side of a binding)
extern VROOM__Object* VROOM__IMMUT__Capture; //    Function call arguments (right-hand side of a binding)
extern VROOM__Object* VROOM__IMMUT__Blob; //       An undifferentiated mass of bits

/*
 * Mutable types
 */
extern VROOM__Object* VROOM__MUTAB__Scalar; //      Perl scalar
extern VROOM__Object* VROOM__MUTAB__Array; //       Perl array
extern VROOM__Object* VROOM__MUTAB__Hash; //        Perl hash
extern VROOM__Object* VROOM__MUTAB__KeyHash; //     Perl hash that autodeletes values matching default
extern VROOM__Object* VROOM__MUTAB__KeySet; //      KeyHash of Bool (does Set in list/array context)
extern VROOM__Object* VROOM__MUTAB__KeyBag; //      KeyHash of UInt (does Bag in list/array context)
extern VROOM__Object* VROOM__MUTAB__Buf; //         Perl buffer (a stringish array of memory locations)
extern VROOM__Object* VROOM__MUTAB__IO; //          Perl filehandle
extern VROOM__Object* VROOM__MUTAB__Routine; //     Base class for all wrappable executable objects
extern VROOM__Object* VROOM__MUTAB__Sub; //         Perl subroutine
extern VROOM__Object* VROOM__MUTAB__Method; //      Perl method
extern VROOM__Object* VROOM__MUTAB__Submethod; //   Perl subroutine acting like a method
extern VROOM__Object* VROOM__MUTAB__Macro; //       Perl compile-time subroutine
extern VROOM__Object* VROOM__MUTAB__Regex; //       Perl pattern
extern VROOM__Object* VROOM__MUTAB__Match; //       Perl match, usually produced by applying a pattern
extern VROOM__Object* VROOM__MUTAB__Package; //     Perl 5 compatible namespace
extern VROOM__Object* VROOM__MUTAB__Module; //      Perl 6 standard namespace
extern VROOM__Object* VROOM__MUTAB__Class; //       Perl 6 standard class namespace
extern VROOM__Object* VROOM__MUTAB__Role; //        Perl 6 standard generic interface/implementation
extern VROOM__Object* VROOM__MUTAB__Grammar; //     Perl 6 pattern matching namespace
extern VROOM__Object* VROOM__MUTAB__Any; //         Perl 6 object (default parameter type, excludes Junction)
extern VROOM__Object* VROOM__MUTAB__Object; //      Perl 6 object (either Any or Junction)

#endif
