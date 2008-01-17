
#ifndef SMOP_STACK_H
#define SMOP_STACK_H

#include <smop_base.h> // this is declared by smop.h which is the
                        // one who includes this file, but let's keep
                        // this here if anyone wants to include only
                        // parts of smop.h. It should do no harm
                        // because of the ifndefs of the beggining of
                        // the file.

/*
 * In SMOP, every low-level opeator is actually a method call in the
 * responder interface of the lowlevel module. For the stack, the
 * SMOP__STACK__Operators type is actually also a responder interface
 * that handles special methods. Calling it from the low-level works
 * the same as calling from the high level. Every operation message is
 * composed by basically three elements:
 *
 *   responder: which responder interface handles this message
 *   identifier: the identifier for the message
 *   capture: the arguments to this message
 *
 * This is valid from bottom up. This way, it's important to make
 * available some identifiers for this operations. The only difference
 * from the lowlevel operations to the high-level is that on the
 * low-level the capture object must be from a known lowlevel type, so
 * when building a lowlevel call in the high-level these specialized
 * objects must be created and not regular capture objects.
 *
 * In order to actually being able to call the operators, the
 * low-level defines some constant objects that will later stringify
 * to the operation name. When in the high-level using them or their
 * stringification is valid, but using the constant values is an
 * optimization as it will result in simple pointer comparisions
 * instead of unicode string comparision.
 *
 * All the lowlevel operator identifiers will have the _OP_ mark in
 * their names. And all the lowlevel metaclasses will have the
 * __Operators sufix.
 *
 * The first challenge on that, is the fact that Capture is also an
 * object, and then you might ask how as we going to create an object
 * if we need an object to create it. And that's where a mix between
 * stack population time and runtime comes to rescue.
 *
 * Let's consider you want to build a stack in the runtime, then
 * you'll need to capture the parameters to the stack operations. You
 * do that by knowing the frame you are creating, and then moving
 * things in the stack to make them available to other calls. As the
 * knowledge of what to move, from where and to where is already
 * available when you are building the frame you'll invoke, you can
 * use at that time some specific objects and build the stack. This
 * specifc objects don't use the SMOP stack, only the C stack which
 * makes you free to call them anytime, by using the lowlevel C
 * subroutine, while the prototype will still support a high-level
 * call that builds the same object.
 *
 */


/* SMOP__STACK__Operators
 *
 * This is the responder interface that support all stack operators as
 * described below.
 *
 * This object is closed and final.
 */
extern SMOP__Object* SMOP__STACK__Operators;

/* The following 5 operators will allways manipulate the current call
 * stack. It is strongly advised that this should only be called by
 * the interpreter loop itself, with a running stack. They are exposed
 * to the high-level space so you can build a new stack referencing
 * them. Calling this directly will probably cause a segfault.
 * This operators are:
 *
 * SMOP__STACK__OP_Forget
 * SMOP__STACK__OP_Move_Capturize
 * SMOP__STACK__OP_Move_Identifier
 * SMOP__STACK__OP_Move_MetaClass
 * SMOP__STACK__OP_Move_Copy
 *
 */

/* SMOP__STACK__OP_Forget
 *
 * This operator simply drop the past nodes of the current stack and
 * let them be destroyed. Can be used by optimizers when return values
 * are no longer needed in a stack frame, causing non-lexical values
 * to be destroyed even before the end of the block.
 *
 * This operator does not receive any parameter. It have a empty
 * signature and may receive a NULL capture.
 */
#define SMOP__STACK__OP_Forget SMOP__ID__forget

/* SMOP__STACK__OP_Move_Capturize
 *
 * Creates a low-level capture from previous stack nodes and store it
 * in a continuation node, supporting the invocant, positional and
 * named arguments. This operator operates in the current stack and
 * calling it directly from the high level seems very
 * innapropriate. The high level is supposed only to use it to define
 * nodes that will be evaluated later. This code will remove the
 * result of the referenced nodes in order to avoid refcount
 * manipulation. If you want to use a result from one node in more
 * than one capture you should use the copy operator.
 *
 * lowlevel C call: smop__stack__opcapture_move_capturize_new
 *
 * This subroutine receives a C int pointing to which past node should
 * the invocant be taken, two C int null-terminated arrays pointing
 * the past nodes that contain each of the positional arguments and
 * each of the pairs that compose the named arguments. This is not the
 * operator call yet, it just creates the capture that can be stored
 * in a stack node for a later call.
 *
 * highlevel capture prototype: SMOP__STACK__OPCAPTURE_Move_Capturize
 *
 * This is the prototype that will be able to create this capture
 * object in the high level. The "new" signature to create this should
 * be (int, List of int, List of int, int). But again, this is just
 * the capture creator for the operator and not the operator itself.
 */
#define SMOP__STACK__OP_Move_Capturize SMOP__ID__move_capturize
SMOP__Object* smop__stack__opcapture_move_capturize_new(int invocant,
                                                          int* positional,
                                                          int* named,
                                                          int target);
extern SMOP__Object* SMOP__STACK__OPCAPTURE_Move_Capturize;

/* SMOP__STACK__OP_Move_Identifier
 *
 * Move the result of a given past node to a target node as the
 * identifier of the message. This operator shouldn't probably be used
 * directly from the high-level as it manipulates the current stack,
 * being referenced in new nodes for later evaluation only. This
 * operator moves the result, if you want to use a result more than
 * once, use the copy operator.
 *
 * lowlevel C call: smop__stack__op_move_identifier_new
 *
 * This subroutine receives two C ints and creates an object that can
 * be used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: SMOP__STACK__OPCAPTURE_Move_Identifier
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will be (int,
 * int).
 */
#define SMOP__STACK__OP_Move_Identifier SMOP__ID__move_identifier
SMOP__Object* smop__stack__opcapture_move_identifier_new(int source, int target);
extern SMOP__Object* SMOP__STACK__OPCAPTURE_Move_Identifier;

/* SMOP__STACK__OP_Move_Responder
 *
 * Move the result of a given past node to a target node as the
 * responder of the message. This operator shouldn't probably be used
 * directly from the high-level as it operates on the current stack,
 * being only referenced in nodes for future evaluation. This operator
 * moves the result, if you want to use a result more than once, use
 * the copy operator.
 *
 * lowlevel C call: smop__stack__opcapture_move_responder_new
 *
 * This subroutine receives two C ints and creates an object that can
 * be used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: SMOP__STACK__OPCAPTURE_Move_Responder
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will be (int,
 * int).
 */
#define SMOP__STACK__OP_Move_Responder SMOP__ID__move_responder
SMOP__Object* smop__stack__opcapture_move_responder_new(int source, int target);
extern SMOP__Object* SMOP__STACK__OPCAPTURE_Move_Responder;

/* SMOP__STACK__OP_Copy
 *
 * This operator copies the result of some past node and returns
 * it. This is usefull when you want the result of some node to be
 * available to more than one of the move operators. This operator
 * manipulates the current stack.
 *
 * lowlevel C call: smop__stack__opcapture_copy_new
 *
 * This subroutine receives a C int and create an object that can be
 * used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: SMOP__STACK__OPCAPTURE_Copy
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will simply be
 * (int).
 */
#define SMOP__STACK__OP_Copy SMOP__ID__copy
SMOP__Object* smop__stack__opcapture_copy_new(int source);
extern SMOP__Object* SMOP__STACK__OPCAPTURE_Copy;


/*
 * The SMOP__STACK__Stack and the SMOP__STACK__Node prototypes are the
 * ones used to create new stacks and to operate on it. Differently
 * from the lowlevel stack operations that manipulate the currently
 * running stack, these methods are safe to be run without a current
 * stack at all. This methods always look for the stack as the
 * invocant in the capture, and not as the stack parameter to the
 * lowlevel MESSAGE call.
 *
 * In fact, when calling the low-level MESSAGE (using SMOP_DISPATCH,
 * probably) you can even pass NULL as the current stack. IF this
 * methods need to recurse they will do it using the C stack. The only
 * exception for this rule is the "eval" method on the Stack
 * prototype. This method will actually call a message using the given
 * stack as the call stack, and probably cause stack manipulation.
 *
 * The methods of this types already use a high-level compatible
 * Capture (as opposed to the lowlevel stack operators that use a
 * different object as the capture), but still some helper C methods
 * are defined here to create the captures, as to make it possible to
 * use these methods from the low-level more easily and efficiently.
 *
 * As with the lowlevel operators, a set of constants is defined as to
 * provide a more optimized method for name resolution for this
 * prototypes. But if that fails, it will fallback to string name
 * resolution.
 */

/* SMOP__STACK__Stack
 *
 * The Stack type is a reference holder to the top-most node in the
 * execution. It works like an iterator on the stack, but it's a live
 * iterator that can change the sequence of the iteration. The idea
 * behind the stack containing just a reference to the current node
 * resides in the fact that this makes very easy to manipulate the
 * current stack and displace the execution to different chains. It
 * could even be used to implement green threads (non-OS threads) like
 * implemeting a POE kernel.
 *
 * The only information the stack itself holds is the reference to the
 * currently selected node. And everything the Stack methods do always
 * use that as the starting point.
 *
 * None of this methods manipulate the current stack, and it is
 * considered legal to pass a NULL reference to the low-level MESSAGE
 * call. None of this methods will recurse, but "eval" will call the
 * current node's message passing the invocant as the call stack.
 *
 * The Stack type is closed and final.
 *
 */
extern SMOP__Object* SMOP__STACK__Stack;

/* SMOP__STACK__Stack_new
 *
 * This method creates a new Stack object. It doesn't receive any
 * argument and simply returns an empty stack for later manipulation.
 *
 * Signature: ()
 *
 * Lowlevel C call: not necessary, you can actually pass
 * NULL as the capture.
 */
#define SMOP__STACK__Stack_new SMOP__ID__new

/* SMOP__STACK__Stack_push
 *
 * This method pushes a given node to the stack. This will cause the
 * following operation:
 *
 * If the stack is empty, the given node is considered the first
 * frame. Remember that the interpreter loop won't eval the pushed
 * node, bacause it calls next before eval, this first node will serve
 * to give information to the current frame, like a capture object.
 *
 * Before
 *                                *
 *    Current Frame     1 <- 2 <- 3 -> 4 -> 5
 *
 * After
 *                                *
 *    New Frame              given node
 *                                |
 *    Current Frame     1 <- 2 <- 3 -> 4 -> 5
 *
 * Signature:
 *     (SMOP__STACK__Stack $stack: SMOP__STACK__Node $node);
 *
 * Lowlevel C call:
 *     smop__stack__stack_push_capture(SMOP__Object* stack,
 *                                     SMOP__Object* node);
 */
#define SMOP__STACK__Stack_push SMOP__ID__push
SMOP__Object* smop__stack__stack_push_capture(SMOP__Object* stack,
                                              SMOP__Object* node);

/* SMOP__STACK__Stack_continues
 *
 * This method adds a node as the continuation of the currently
 * selected node of the stack.
 *
 * Before
 *                              * 
 *    Current Frame   1 <- 2 <- 3 -> 4 -> 5
 *
 * After
 *                              * 
 *    Current Frame   1 <- 2 <- 3 -> given node -> 4 -> 5
 *
 * Signature:
 *     (SMOP__STACK__Stack $stack: SMOP__STACK__Node $node);
 *
 * Lowlevel C call:
 *     smop__stack__stack_continues_capture(SMOP__Object* stack,
 *                                          SMOP__Object* node);
 * 
 */
#define SMOP__STACK__Stack_continues SMOP__ID__continues
SMOP__Object* smop__stack__stack_continues_capture(SMOP__Object* stack,
                                                   SMOP__Object* node);

/* SMOP__STACK__Stack_next
 *
 * This method goes to the next node in the stack, with implied stack
 * droppings being realised.
 *
 * This have some situations:
 *
 *      *                            *
 * 1 <- 2 -> 3  turns into 1 <- 2 <- 3
 *      |                            |
 *
 *           *
 * 1 <- 2 <- 3  turns into 
 *           |                  *
 *      1 <- 2             1 <- 2
 *
 * where the result from the last node is copied as the result of the
 * outer node, and the inner frame is released, which will cause it to
 * be got by the gc, unless of course, the frame is saved somewhere
 * else. This method should always leave the selected node as an
 * executable node, unless the stack becomes empty.
 *
 * Nodes without a responder are considered NO-OPs. They are usefull
 * for boxing arguments to capturize in future nodes. You just need to
 * store it as the return value.
 *
 * Signature:
 *     (SMOP__STACK__Stack $stack: )
 *
 * Lowlevel C call:
 *     smop__stack__stack_next_capture(SMOP__Object* stack);
 */
#define SMOP__STACK__Stack_next SMOP__ID__next
SMOP__Object* smop__stack__stack_next_capture(SMOP__Object* stack);

/* SMOP__STACK__Stack_current
 *
 * Returns the currently selected node.
 *
 * Signature:
 *     (SMOP__STACK__Stack $stack: )
 *
 * Lowlevel C call:
 *     smop__stack__stack_current_capture(SMOP__Object* stack);
 */
#define SMOP__STACK__Stack_current SMOP__ID__current
SMOP__Object* smop__stack__stack_current_capture(SMOP__Object* stack);

/* SMOP__STACK__Stack_goto
 *
 * Sets the currently selected node.
 *
 * Note that, inside a interpreter loop, this will change the current
 * node, which means that the node you're passing will not be
 * evaluated. Use it to pass information to that frame (like the
 * return point in case of continuation-passing-style).
 *
 * Signature:
 *     (SMOP__STACK__Stack $stack: SMOP__STACK__Node $node)
 *
 * Lowlevel C call:
 *     smop__stack__stack_goto_capture(SMOP__Object* stack, SMOP__Object* node);
 */
#define SMOP__STACK__Stack_goto SMOP__ID__goto
SMOP__Object* smop__stack__stack_goto_capture(SMOP__Object* stack, SMOP__Object* node);

/* SMOP__STACK__Stack_eval
 *
 * Evaluate the selected node, and only the selected node, storing the
 * result in the node. This method won't recurse in the C stack, it
 * will, in the case this node would cause a C recursion, push
 * something to the given stack. In a very clear vision, it calls the
 * message giving it the invocant stack as the call stack. It returns
 * the return value of the node.
 *
 * Signature:
 *     (SMOP__STACK__Stack $stack: )
 * Lowlevel C call:
 *     smop__stack__stack_eval_capture(SMOP__Object* stack);
 */
#define SMOP__STACK__Stack_eval SMOP__ID__eval
SMOP__Object* smop__stack__stack_current_capture(SMOP__Object* stack);

/* SMOP__STACK__Stack_result
 *
 * Returns the result of some past node, counting backwards. This
 * means that 1 is the immediate past node, 2 is that nodes past node
 * and so on.
 *
 * Signature:
 *     (SMOP__STACK__Stack $stack: int node)
 * Lowlevel C call:
 *     smop__stack__stack_result_capture(SMOP__Object* stack, int node);
 */
#define SMOP__STACK__Stack_result SMOP__ID__result
SMOP__Object* smop__stack__stack_result_capture(SMOP__Object* stack, int node);

/* SMOP__STACK__Stack_has_next
 *
 * Returns true if the stack is not empty and false if it is.
 *
 * Signature:
 *     (SMOP__STACK__Stack $stack: )
 * Lowlevel C call:
 *     smop__stack__stack_has_next_capture(SMOP__Object* stack);
 */
#define SMOP__STACK__Stack_has_next SMOP__ID__has_next
SMOP__Object* smop__stack__stack_has_next_capture(SMOP__Object* stack);

/* SMOP__STACK__Stack_loop
 *
 * This is the only method that will use the C stack. It will return
 * when the stack gets empty, it will iterate in the given stack using
 * has_hext, next and eval until the stack is empty. The return value
 * will be the return value of the last node evaled in the stack.
 *
 * Signature:
 *     (SMOP__STACK__Stack $stack: )
 * Lowlevel C call:
 *     smop__stack__stack_loop_capture(SMOP__Object* stack);
 */
#define SMOP__STACK__Stack_loop SMOP__ID__loop
SMOP__Object* smop__stack__stack_loop_capture(SMOP__Object* stack);

/* SMOP__STACK__Node
 *
 * The node type is just a placeholder for the data operated by the
 * stack. It's a simple object that have simple accessors. These are
 * not the standard Perl accessors, are explicit methods that handle
 * it.
 *
 * One way or another, this class is closed and final.
 */
extern SMOP__Object* SMOP__STACK__Node;

/* SMOP__STACK__Node_new
 *
 * This creates a new Node object.
 *
 * Signature:
 *    (:$responder, :$identifier, :$capture, :$debug,
 *     :$jail, :$lexical, :$outer, :$continuation,
 *     :$past, :$result)
 *
 * Lowlevel C call: (may receive NULL)
 *     smop__stack__node_new_capture(
 *                    SMOP_Object* responder,
 *                    SMOP_Object* identifier,
 *                    SMOP_Object* capture,
 *                    SMOP_Object* debug,
 *                    SMOP_Object* jail,
 *                    SMOP_Object* lexical,
 *                    SMOP_Object* outer,
 *                    SMOP_Object* continuation,
 *                    SMOP_Object* past,
 *                    SMOP_Object* result)
 */
#define SMOP__STACK__Node_new SMOP__ID__new
SMOP__Object* smop__stack__node_new_capture(SMOP_Object* responder,
                                             SMOP_Object* identifier,
                                             SMOP_Object* capture,
                                             SMOP_Object* debug,
                                             SMOP_Object* jail,
                                             SMOP_Object* lexical,
                                             SMOP_Object* outer,
                                             SMOP_Object* continuation,
                                             SMOP_Object* past,
                                             SMOP_Object* result);

/* All the Node accessor methods have the same simple signature,
 * which is:
 *
 *  ($node: $newvalue? )
 *
 * Usually the accessor methods would have an "is rw" attribute,
 * instead of optionally receiving the new value. This is a special
 * case, and is made that way to simplify the usage of this methods in
 * the stack level, once that "is rw" would have to imply scalar
 * context.
 *
 * And the same lowlevel C call (may receive NULL in newvalue):
 *
 *  smop__stack__node_accessor_capture(SMOP__Object* node,
 *                                      SMOP__Object* newvalue);
 */
SMOP__Object* smop__stack__node_accessor_capture(SMOP__Object* node,
                                                   SMOP__Object* newvalue);

/* SMOP__STACK__Node_responder
 *
 * The responder interface that will answer to this node.
 */
#define SMOP__STACK__Node_responder SMOP__ID__responder

/* SMOP__STACK__Node_identifier
 *
 * The identifier of the message
 */
#define SMOP__STACK__Node_identifier SMOP__ID__identifier

/* SMOP__STACK__Node_capture
 *
 * The arguments of the message
 */
#define SMOP__STACK__Node_capture SMOP__ID__capture

/* SMOP__STACK__Node_debug
 *
 * Debug information about the node
 */
#define SMOP__STACK__Node_debug SMOP__ID__debug

/* SMOP__STACK__Node_jail
 * 
 * Jail for exception and other operations that might unwind the
 * stack. This is not a flag, but an object that may be looked while
 * unwinding the stack. Nothing says that the operator *must* stop on
 * any jail.
 */
#define SMOP__STACK__Node_jail SMOP__ID__jail

/* SMOP__STACK__Node_lexical
 *
 * The reference to the lexical scope in this node.
 */
#define SMOP__STACK__Node_lexical SMOP__ID__lexical

/* SMOP__STACK__Node_outer
 *
 * The currently selected node in the outer frame.
 */
#define SMOP__STACK__Node_outer SMOP__ID__outer

/* SMOP__STACK__Node_continuation
 *
 * The continuation of this node
 */
#define SMOP__STACK__Node_continuation SMOP__ID__continuation

/* SMOP__STACK__Node_past
 *
 * The node that was executed before this one
 */
#define SMOP__STACK__Node_past SMOP__ID__past

/* SMOP__STACK__Node_result
 *
 * The result evaluation of this node
 */
#define SMOP__STACK__Node_result SMOP__ID__result


#endif
