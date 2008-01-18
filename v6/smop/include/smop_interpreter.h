#ifndef SMOP_INTERPRETER_H
#define SMOP_INTERPRETER_H

/* The smop interpreter instance is the object that represents a line
 * of execution. Here the API for code execution in SMOP is defined,
 * It is essentially an API that must be implemented by any
 * interpreter implementation. The interpreter instance just delegates
 * to the current interpreter implementation.
 *
 * The default smop interpreter implementation is in
 * smop_stack.h. It's a node-based stack that supports
 * continuations. If you want to use that implementation in your C
 * program, you need to include that header in specific.
 * 
 * This API is based simply in the idea that you have a interpreter
 * instance object that points to a continuation (the current state of
 * some interpreter implementation). This object represents the
 * current execution. There can be many instances (to implement green
 * threads or OS threads, for instance) at the same time.
 *
 * The implementation API is simply composed by the methods that this
 * object delegates to the continuation. With the exception of
 * has_next which is evaluated in boolean context, every other method
 * is evaluated in void context.
 * 
 */

/* This is the default interpreter prototype. There can be more than
 * one during an execution, but this will result in C recursion, as
 * there's no way to change the current interpreter, although this
 * instance just points to the current implementation instance, that
 * is the one supposed to actually do something, so there's probably
 * not much point in having another interpreter instance
 * implementation.
 */
SMOP__Object* SMOP__INTPTR__InterpreterInstance;


/* The interpreter instance have the following methods (invocant implied):
 *
 * For every method, there's a low-level c call to create a valid
 * capture to that call. The identifiers for the methods can be taken
 * from the smop_identifiers.h to be used.
 *
 * For all the methods that the signature only means the invocant,
 * this call should do the trick. You can pass null to the second
 * argument if you don't want to have a initialized continuation.
 * smop__intptr__invocant_capture_new(SMOP__Object* obj, SMOP__Object*
 * cont);
 */
SMOP__Object* smop__intptr__invocant_capture_new(SMOP__Object* proto, SMOP__Object* continuation);

/* new()
 *
 * This method creates a new interpreter instance with no continuation
 * associated.
 */

/* goto($new_continuation)
 *
 * This method sets the continuation of this instance. It's important
 * to remember that the interpreter loop is "next(); eval();", so most
 * of the cases the first operation is never called.
 *
 * lowlevel C call:
 * smop__intptr__goto_capture_new(SMOP__Object* invocant, SMOP__Object* continuation);
 */
SMOP__Object* smop__intptr__goto_capture_new(SMOP__Object* invocant, SMOP__Object* continuation);

/* eval()
 *
 * This method delegates to the current continuation.
 */

/* has_next()
 *
 * This method delegates to the current continuation.
 */

/* next()
 *
 * This method delegates to the current continuation.
 */

/* loop()
 *
 * This method will keep doing "next(); eval();" while
 * "has_next();".
 */

#endif
