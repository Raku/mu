
#ifndef SMOP_LOWLEVEL_H
#define SMOP_LOWLEVEL_H

#include <smop_base.h>

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
extern void smop_lowlevel_destroy();

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
extern SMOP__Object* smop_lowlevel_refcnt_inc(SMOP__Object* stack, SMOP__Object* value);

/* This functions decrements the reference count of a value, it should
 * be called whenever one reference to this value is destroyed. It
 * will call DESTROYALL and free() the pointer when appropriate. It
 * always return the input pointer. Note that the call to DESTROYALL
 * will be made using the given stack, so this will use a
 * continuation-passing-style to 1) call DESTROYALL, 2) free the
 * pointer and 3) return the stack to where it was.
 *
 * One other interesting thing is, when several objects loose the last
 * reference, the continuations will be chained for the destruction.
 *
 * For the sake of documentation, here goes the code equivalent to
 * what happens with the stack during destruction. Considering $?STACK
 * as the current stack and $obj as the object being destroyed.
 *
 * my $continuation = ___STACK___.current();
 * my $first_node = Node.new(result => $continuation);
 * my $second_node = Node.new(responder => ___RI___($obj),
 *                            identifier => "DESTROYALL",
 *                            capture => \($obj: ));
 * $first_node.continuation($second_node);
 * my $third_node = Node.new(responder => SMOP_LOWLEVEL,
 *                           identifier => "FREE",
 *                           capture => \($obj ));
 * $second_node.continuation($third_node);
 * my $fourth_node = Node.new(result => ___STACK___);
 * $third_node.continuation($fourth_node);
 * my $fifth_node = Node.new(responder => SMOP__STACK__Operators,
 *                           identifier => SMOP__STACK__OP_Move_Capturize,
 *        capture => SMOP__STACK__OPCAPTURE_Move_Capturize.new(1,(4),(),1));
 * $fourth_node.continuation($fifth_node);
 * my $sixth_node = Node.new(responder => ___RI___(___STACK___),
 *                           identifier => "goto");
 * $fifth_node.continuation($sixth_node);
 * ___STACK___.goto($first_node);
 *
 */
extern SMOP__Object* smop_lowlevel_refcnt_dec(SMOP__Object* stack, SMOP__Object* value);

/* This functions synchronizes the access to this value. It should be
 * called whenever some pointer in the low-level details (some
 * non-core-value member of the struct) will be accessed. No recursion
 * should be made while a object is locked, this lock is a lowlevel
 * lock to avoid segfaults when multithreaded, not a user-level lock.
 */
extern void smop_lowlevel_rdlock(SMOP__Object* value);
extern void smop_lowlevel_wrlock(SMOP__Object* value);
extern void smop_lowlevel_unlock(SMOP__Object* value);


#endif
