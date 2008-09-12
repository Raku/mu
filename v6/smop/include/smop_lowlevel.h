
#ifndef SMOP_LOWLEVEL_H
#define SMOP_LOWLEVEL_H

#include <smop.h>
#include <smop_slime.h>
#include <pthread.h>

/* The lowlevel SMOP API is an additional API that is not included by
 * the smop.h file, because this functions only need to be seen by
 * code that is implementing a ResponderInterface which uses the same
 * lowlevel object management than SMOP. This is not mandatory, any
 * binary-compatible implementation is valid.
 *
 * This lowlevel implementation uses a simple refcount gc and a
 * pthread_rwlock for thread safety while mangling the object
 * internals.
 *
 */

extern void smop_lowlevel_init();
extern void smop_lowlevel_destr();

typedef struct SMOP_LOWLEVEL_INTERNAL {
  int ref_cnt;
  pthread_rwlock_t* rwlock;
} SMOP_LOWLEVEL_INTERNAL;

/* This function is the place from where every allocation should
 * happen.
 *
 * The data member on the basic structure is reserved for the lowlevel
 * management. You should declare a binary-compatible structure for
 * your objects that extends the basic SMOP__Object to have more
 * information into your object.
 */
extern SMOP__Object* smop_lowlevel_alloc(int size);

/* This function increments the reference count of a value, it should
 * be called whenever the value is referenced by another value, and
 * always return the input pointer.
 */
extern SMOP__Object* smop_lowlevel_refcnt_inc(SMOP__Object* interpreter, SMOP__ResponderInterface* ri, SMOP__Object* value);

SMOP__Object* SMOP__RI__create(
  SMOP__Object* (*MESSAGE)  (SMOP__Object* interpreter,           
                             SMOP__ResponderInterface* self,      
                             SMOP__Object* identifier,            
                             SMOP__Object* capture),              
  SMOP__Object* (*REFERENCE)(SMOP__Object* interpreter,           
                             SMOP__ResponderInterface* self,      
                             SMOP__Object* object),               
  SMOP__Object* (*RELEASE)  (SMOP__Object* interpreter,           
                             SMOP__ResponderInterface* self,      
                             SMOP__Object* object),
  char *id
); 

/* This functions decrements the reference count of a value, it should
 * be called whenever one reference to this value is destroyed. It
 * will call DESTROYALL and free() the pointer when appropriate. It
 * always return the input pointer. Note that the call to DESTROYALL
 * will be made using the given interpreter, so this will use a
 * continuation-passing-style to 1) call DESTROYALL, 2) free the
 * pointer and 3) return the interpreter to where it was.
 *
 * One other interesting thing is, when several objects loose the last
 * reference, the continuations will be chained for the destruction.
 *
 * For the sake of documentation, here goes the code equivalent to
 * what happens with the interpreter during an lowlevel based object
 * destruction.
 *

# sm0p quoting returns a smop stack of the literal translation of the
# code to a set of nodes. (one statement = one node). The
# implementation used here and set as the continuation is the one
# defined in smop_stack.h

my $obj = shift; # object being freed.
my $current = ___INTERPRETER___.current;
goto ___INTERPRETER___: q:sm0p {
   $current;
   ___INTERPRETER___;
   $obj.DESTROYALL();
   SMOP__STACK__Operators.move_capturize(
       |SMOP__STACK__OPCAPTURE_Move_Capturize.new(2,(3),(),3));
   SMOP__STACK__Operators.forget();
   SMOP__STACK__Operators.free(|$obj);
   ___INTERPRETER___.goto()
};

 *
 * One important thing to notice here is that this would create a deep
 * recursion, as the object destruction code cause the destruction of
 * the node objects (the forget op). But this doesn't happen because
 * the Stack and the Node have custom Responder Interfaces that
 * doesn't call this code.
 */
extern SMOP__Object* smop_lowlevel_refcnt_dec(SMOP__Object* interpreter, SMOP__ResponderInterface* ri, SMOP__Object* value);
extern SMOP__Object* smop_lowlevel_generic_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj);
extern SMOP__Object* smop_lowlevel_generic_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj); 

/* This functions synchronizes the access to this value. It should be
 * called whenever some pointer in the low-level details (some
 * non-core-value member of the struct) will be accessed. No recursion
 * should be made while a object is locked, this lock is a lowlevel
 * lock to avoid segfaults when multithreaded, not a user-level lock.
 */
extern void smop_lowlevel_rdlock(SMOP__Object* value);
extern void smop_lowlevel_wrlock(SMOP__Object* value);
extern void smop_lowlevel_unlock(SMOP__Object* value);

/*
 * In order to enable the working of some features, Some lowlevel
 * methods need to be exposed to the high-level. At this point, only
 * the FREE method appears to be needed, as to implement the CPS
 * object destruction.
 *
 * Following the other operators, we'll have a responder interface and
 * some constant names to implement it.
 */
extern SMOP__Object* SMOP__LOWLEVEL__Operators;

/* SMOP__LOWLEVEL__Operators.free(|$obj);
 *
 * This operator make a pointer freed. This is an extremely low-level
 * operator that is only available as the destroy code can be called
 * using the stack. It **IGNORES** the current refcount of an object,
 * you should only call it after being sure this object can be really
 * freed.
 *
 * This special operator will explicitly manipulate the stack to
 * remove the reference to the object in the capture member of the
 * node, as no call to RELEASE should be tried afterwards.
 */

#endif
