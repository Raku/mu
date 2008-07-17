#ifndef SMOP_NATIVE_H
#define SMOP_NATIVE_H

/* See http://www.perlfoundation.org/perl6/index.cgi?smop_native_types
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
extern SMOP__Object* SMOP__NATIVE__bool_true;
extern SMOP__Object* SMOP__NATIVE__bool_false;
extern SMOP__Object* SMOP__NATIVE__capture;

// create methods
SMOP__Object*   SMOP__NATIVE__idconst_create(const char* value);
SMOP__Object*   SMOP__NATIVE__idconst_createn(const char* value, int size);
SMOP__Object*   SMOP__NATIVE__bytes_create(char* value, int size);
SMOP__Object*   SMOP__NATIVE__bit_create(int value);
SMOP__Object*   SMOP__NATIVE__int_create(int value);
SMOP__Object*   SMOP__NATIVE__uint_create(unsigned int value);
SMOP__Object*   SMOP__NATIVE__buf_create(int bytesize, char* unicodestr);
SMOP__Object*   SMOP__NATIVE__num_create(double value);
SMOP__Object*   SMOP__NATIVE__complex_create(double complex value);
SMOP__Object*   SMOP__NATIVE__bool_create(int value);
SMOP__Object*   SMOP__NATIVE__capture_create(SMOP__Object* interpreter,
                                             SMOP__Object* invocant,
                                             SMOP__Object** positional,
                                             SMOP__Object** named);
/* The delegate method is a helper that takes a capture, creates a
 * clone of it, and then sets the passed invocant as the capture's
 * invocant and returns it.
 */
SMOP__Object*   SMOP__NATIVE__capture_delegate(SMOP__Object* interpreter,
                                               SMOP__Object* invocant,
                                               SMOP__Object* original_capture);

// get methods
char*           SMOP__NATIVE__idconst_fetch(SMOP__Object* value, int* retsize);
char*           SMOP__NATIVE__bytes_fetch(SMOP__Object* value, int* retsize);
int             SMOP__NATIVE__bit_fetch(SMOP__Object* value);
int             SMOP__NATIVE__int_fetch(SMOP__Object* value);
unsigned int    SMOP__NATIVE__uint_fetch(SMOP__Object* value);
char*           SMOP__NATIVE__buf_fetch(SMOP__Object* value, int* retsize);
double          SMOP__NATIVE__num_fetch(SMOP__Object* value);
complex  SMOP__NATIVE__complex_fetch(SMOP__Object* value);
int             SMOP__NATIVE__bool_fetch(SMOP__Object* value);
int             SMOP__NATIVE__capture_may_recurse(SMOP__Object* interpreter,
                                                  SMOP__Object* capture);
SMOP__Object*   SMOP__NATIVE__capture_invocant(SMOP__Object* interpreter,
                                               SMOP__Object* capture);
int             SMOP__NATIVE__capture_positional_count(SMOP__Object* interpreter,
                                                       SMOP__Object* capture);
SMOP__Object*   SMOP__NATIVE__capture_positional(SMOP__Object* interpreter,
                                                 SMOP__Object* capture, int p);
SMOP__Object*   SMOP__NATIVE__capture_named(SMOP__Object* interpreter,
                                            SMOP__Object* capture,
                                            SMOP__Object* identifier);

// idconst isn't subject to gc, you need a explicit free
void            SMOP__NATIVE__idconst_free(SMOP__Object* value);


#endif
