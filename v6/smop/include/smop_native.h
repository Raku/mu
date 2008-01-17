#ifndef SMOP_NATIVE_H
#define SMOP_NATIVE_H

/*
 * Besides the basic structures to which all objects must be
 * binary-compatible with, we also need to have defined which are the
 * native types for SMOP. These types are the key for the SMOP runtime
 * being able to actually do something in the low-level. The key to
 * that is in S12:
 *
 *       You may derive from any built-in type, but the derivation of
 *       a low-level type like int may only add behaviors, not change
 *       the representation. Use composition and/or delegation to
 *       change the representation.
 *
 * This means that the native objects can have a fixed structure. As
 * they are known and fixed, we can intermix high-level calls and
 * low-level calls. An important thing then, is that the autoboxing
 * from the native types to the immutable types must be available in
 * both ways, as the non-native implementations may even be some
 * object that acts like a Int, so we can only use it by calling the
 * metaclass.
 *
 * This means that, besides having a way to enforce numeric context to
 * all values, we need a way to also force native-type-context to that
 * value. Which would be something like:
 *
 *       my $a = +$someobject; # Returns Int, Num, Complex or even Rat
 *       my $b = $a.$native_int_method(); # returns that as int
 *       my $b = $a.$native_float_method(); # returns that as float
 *
 * The thing is, this methods will be part of the API of any object
 * that emulates one of the types that can be autoboxed to/from native
 * types.  Considering that a Int is any object that returns true to
 * .^does(Int), the low-level runtime cannot presume to know which is
 * the lowlevel implementation of an object, the only that knows it is
 * the responder interface, so it's natural that this native-type
 * coercion methods reside in the responder interface. This way, SMOP
 * will count on that to provide a "native" method that receives the
 * prototype of the native-type to convert to and returns a
 * native-type object. One, possibly more important, reason for the
 * "native" method to reside in the responder interface is because a
 * responder interface will require at least some C code, and creating
 * the lowlevel objects are C calls, so we concentrate that there.
 *
 * But this doesn't mean that every responder interface must implement
 * it directly. The call to native may result in a call to coerce to
 * the respective high-level type before.
 *
 * It's important to realize that the native-type objects are binary
 * compatible with any other object, but as they can't have they
 * representation changed, they can't be extended, and the only
 * prototype that can answer true to .^does(int) is the lowlevel int
 * implementation. It is considered illegal to answer true to that for
 * any other prototype, as this would certainly cause a segfault.
 */


/*
 * The native types are then declared here for external use.
 */
#include <complex.h>
#include <smop_base.h> // this is declared by smop.h which is the
                        // one who includes this file, but let's keep
                        // this here if anyone wants to include only
                        // parts of smop.h. It should do no harm
                        // because of the ifndefs of the beggining of
                        // the file.

// Native operators metaclass proto-object.
extern SMOP__Object* SMOP__NATIVE__Operators;

// prototypes
extern SMOP__Object* SMOP__NATIVE__idconst;
extern SMOP__Object* SMOP__NATIVE__bytes;
extern SMOP__Object* SMOP__NATIVE__bit;
extern SMOP__Object* SMOP__NATIVE__int;
extern SMOP__Object* SMOP__NATIVE__uint;
extern SMOP__Object* SMOP__NATIVE__buf;
extern SMOP__Object* SMOP__NATIVE__num;
extern SMOP__Object* SMOP__NATIVE__complex;
extern SMOP__Object* SMOP__NATIVE__bool;

// create methods
extern SMOP__Object*   SMOP__NATIVE__idconst_create(char* value);
extern SMOP__Object*   SMOP__NATIVE__bytes_create(char* value, int size);
extern SMOP__Object*   SMOP__NATIVE__bit_create(int value);
extern SMOP__Object*   SMOP__NATIVE__int_create(int value);
extern SMOP__Object*   SMOP__NATIVE__uint_create(unsigned int value);
extern SMOP__Object*   SMOP__NATIVE__buf_create(int bytesize, char* unicodestr);
extern SMOP__Object*   SMOP__NATIVE__num_create(double value);
extern SMOP__Object*   SMOP__NATIVE__complex_create(double complex value);
extern SMOP__Object*   SMOP__NATIVE__bool_create(int value);

// get methods
extern char*            SMOP__NATIVE__idconst_fetch(SMOP__Object* value);
extern char*            SMOP__NATIVE__bytes_fetch(SMOP__Object* value, int* retsize);
extern int              SMOP__NATIVE__bit_fetch(SMOP__Object* value);
extern int              SMOP__NATIVE__int_fetch(SMOP__Object* value);
extern unsigned int     SMOP__NATIVE__uint_fetch(SMOP__Object* value);
extern char*            SMOP__NATIVE__buf_fetch(SMOP__Object* value, int* retsize);
extern double           SMOP__NATIVE__num_fetch(SMOP__Object* value);
extern double complex   SMOP__NATIVE__complex_fetch(SMOP__Object* value);
extern int              SMOP__NATIVE__bool_fetch(SMOP__Object* value);

#endif
