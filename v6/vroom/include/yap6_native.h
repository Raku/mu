#ifndef YAP6_NATIVE_H
#define YAP6_NATIVE_H

/*
 * Besides the basic structures to which all objects must be
 * binary-compatible with, we also need to have defined which are the
 * native types for YAP6. These types are the key for the YAP6 runtime
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
 * the metaclass, so it's natural that this native-type coercion
 * methods reside in the metaclass. This way, YAP6 will count on the
 * metaclasses to provide a "native" method that receives the
 * prototype of the native-type to convert to and returns a
 * native-type object. One, possibly more important, reason for the
 * "native" method to reside in the metaclass is because a metaclass
 * will require at least some C code, and creating the lowlevel
 * objects are C calls, so we concentrate that on the metaclass.
 *
 * But this doesn't mean that every metaclass must implement it
 * directly. The call to native may result in a call to coerce to the
 * respective high-level type before.
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
#include <yap6_base.h> // this is declared by yap6.h which is the one
                       // who includes this file, but let's keep this
                       // here if anyone wants to include only parts
                       // of yap6.h. It should do no harm because of
                       // the ifndefs of the beggining of the file.

// Native operators metaclass proto-object.
extern YAP6__Prototype* YAP6__NATIVE__Operators;

// prototypes
extern YAP6__Prototype* YAP6__NATIVE__bit;
extern YAP6__Prototype* YAP6__NATIVE__int;
extern YAP6__Prototype* YAP6__NATIVE__uint;
extern YAP6__Prototype* YAP6__NATIVE__buf;
extern YAP6__Prototype* YAP6__NATIVE__num;
extern YAP6__Prototype* YAP6__NATIVE__complex;
extern YAP6__Prototype* YAP6__NATIVE__bool;

// create methods
extern YAP6__Object*    YAP6__NATIVE__bit_create(int value);
extern YAP6__Object*    YAP6__NATIVE__int_create(int value);
extern YAP6__Object*    YAP6__NATIVE__uint_create(unsigned int value);
extern YAP6__Object*    YAP6__NATIVE__buf_create(int bytesize, char* unicodestr);
extern YAP6__Object*    YAP6__NATIVE__num_create(double value);
extern YAP6__Object*    YAP6__NATIVE__complex_create(double complex value);
extern YAP6__Object*    YAP6__NATIVE__bool_create(int value);

// get methods
extern int              YAP6__NATIVE__bit_fetch(YAP6__Object* value);
extern int              YAP6__NATIVE__int_fetch(YAP6__Object* value);
extern unsigned int     YAP6__NATIVE__uint_fetch(YAP6__Object* value);
extern char*            YAP6__NATIVE__buf_fetch(YAP6__Object* value, int* retsize);
extern double           YAP6__NATIVE__num_fetch(YAP6__Object* value);
extern double complex   YAP6__NATIVE__complex_fetch(YAP6__Object* value);
extern int              YAP6__NATIVE__bool_fetch(YAP6__Object* value);

// set methods
extern void             YAP6__NATIVE__bit_store(YAP6__Object* value, int newvalue);
extern void             YAP6__NATIVE__int_store(YAP6__Object* value, int newvalue);
extern void             YAP6__NATIVE__uint_store(YAP6__Object* value, unsigned int newvalue);
extern void             YAP6__NATIVE__buf_store(YAP6__Object* value, int newbytesize, char* newvalue);
extern void             YAP6__NATIVE__num_store(YAP6__Object* value, double newvalue);
extern void             YAP6__NATIVE__complex_store(YAP6__Object* value, double complex newvalue);
extern void             YAP6__NATIVE__bool_store(YAP6__Object* value, int newvalue);

#endif
