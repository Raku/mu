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
extern SMOP__Object* SMOP__INTPTR__InterpreterInstance;


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
 */

/* setr($new_value)
 *
 * As in
 * http://www.perlfoundation.org/perl6/index.cgi?smop_inter_continuation_communication
 * This sets the evaluation result of the current execution
 * point. This is a delegated call also.
 */

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
