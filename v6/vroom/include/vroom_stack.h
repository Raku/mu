
#ifndef VROOM_STACK_H
#define VROOM_STACK_H

#include <vroom_base.h> // this is declared by vroom.h which is the
                        // one who includes this file, but let's keep
                        // this here if anyone wants to include only
                        // parts of vroom.h. It should do no harm
                        // because of the ifndefs of the beggining of
                        // the file.

/*
 * In VROOM, every low-level opeator is actually a method call in the
 * responder interface of the lowlevel module. For the stack, the
 * VROOM__STACK__Operators type is actually also a responder interface
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
 * specifc objects don't use the VROOM stack, only the C stack which
 * makes you free to call them anytime, by using the lowlevel C
 * subroutine, while the prototype will still support a high-level
 * call that builds the same object.
 *
 */


/* VROOM__STACK__Operators
 *
 * This is the responder interface that support all stack operators as
 * described below.
 *
 * This object is closed and final.
 */
extern VROOM__Object* VROOM__STACK__Operators;

/* The following 4 operators will allways manipulate the current call
 * stack. It is strongly advised that this should only be called by
 * the interpreter loop itself, with a running stack. They are exposed
 * to the high-level space so you can build a new stack referencing
 * them. Calling this directly will probably cause a segfault.
 * This operators are:
 *
 * VROOM__STACK__OP_Move_Capturize
 * VROOM__STACK__OP_Move_Identifier
 * VROOM__STACK__OP_Move_MetaClass
 * VROOM__STACK__OP_Move_Copy
 *
 */
/* VROOM__STACK__OP_Move_Capturize
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
 * lowlevel C call: vroom__stack__opcapture_move_capturize_new
 *
 * This subroutine receives a C int pointing to which past node should
 * the invocant be taken, two C int null-terminated arrays pointing
 * the past nodes that contain each of the positional arguments and
 * each of the pairs that compose the named arguments. This is not the
 * operator call yet, it just creates the capture that can be stored
 * in a stack node for a later call.
 *
 * highlevel capture prototype: VROOM__STACK__OPCAPTURE_Move_Capturize
 *
 * This is the prototype that will be able to create this capture
 * object in the high level. The "new" signature to create this should
 * be (int, List of int, List of int, int). But again, this is just
 * the capture creator for the operator and not the operator itself.
 */
extern VROOM__Object* VROOM__STACK__OP_Move_Capturize;
VROOM__Object* vroom__stack__opcapture_move_capturize_new(int invocant,
                                                          int** positional,
                                                          int** named,
                                                          int target);
extern VROOM__Object* VROOM__STACK__OPCAPTURE_Move_Capturize;

/* VROOM__STACK__OP_Move_Identifier
 *
 * Move the result of a given past node to a target node as the
 * identifier of the message. This operator shouldn't probably be used
 * directly from the high-level as it manipulates the current stack,
 * being referenced in new nodes for later evaluation only. This
 * operator moves the result, if you want to use a result more than
 * once, use the copy operator.
 *
 * lowlevel C call: vroom__stack__op_move_identifier_new
 *
 * This subroutine receives two C ints and creates an object that can
 * be used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: VROOM__STACK__OPCAPTURE_Move_Identifier
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will be (int,
 * int).
 */
extern VROOM__Object* VROOM__STACK__OP_Move_Identifier;
VROOM__Object* vroom__stack__opcapture_move_identifier_new(int source, int target);
extern VROOM__Object* VROOM__STACK__OPCAPTURE_Move_Identifier;

/* VROOM__STACK__OP_Move_MetaClass
 *
 * Move the result of a given past node to a target node as the
 * metaclass of the message. This operator shouldn't probably be used
 * directly from the high-level as it operates on the current stack,
 * being only referenced in nodes for future evaluation. This operator
 * moves the result, if you want to use a result more than once, use
 * the copy operator.
 *
 * lowlevel C call: vroom__stack__opcapture_move_metaclass_new
 *
 * This subroutine receives two C ints and creates an object that can
 * be used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: VROOM__STACK__OPCAPTURE_Move_MetaClass
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will be (int,
 * int).
 */
extern VROOM__Object* VROOM__STACK__OP_Move_MetaClass;
VROOM__Object* vroom__stack__opcapture_move_metaclass_new(int source, int target);
extern VROOM__Object* VROOM__STACK__OPCAPTURE_Move_MetaClass;

/* VROOM__STACK__OP_Copy
 *
 * This operator copies the result of some past node and returns
 * it. This is usefull when you want the result of some node to be
 * available to more than one of the move operators. This operator
 * manipulates the current stack.
 *
 * lowlevel C call: vroom__stack__opcapture_copy_new
 *
 * This subroutine receives a C int and create an object that can be
 * used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: VROOM__STACK__OPCAPTURE_Copy
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will simply be
 * (int).
 */
extern VROOM__Object* VROOM__STACK__OP_Copy;
VROOM__Object* vroom__stack__opcapture_copy_new(int source);
extern VROOM__Object* VROOM__STACK__OPCAPTURE_Copy;


/*
 * The VROOM__STACK__Stack and the VROOM__STACK__Node prototypes are the
 * ones used to create new stacks and to operate on it. Differently
 * from the lowlevel stack operations that manipulate the currently
 * running stack, these methods are safe to be run without a current
 * stack at all. This methods always look for the stack as the
 * invocant in the capture, and not as the stack parameter to the
 * lowlevel MESSAGE call.
 *
 * In fact, when calling the low-level MESSAGE (using VROOM_DISPATCH,
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

/* VROOM__STACK__Stack
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
extern VROOM__Object* VROOM__STACK__Stack;

/* VROOM__STACK__Stack_new
 *
 * This method creates a new Stack object. It doesn't receive any
 * argument and simply returns an empty stack for later manipulation.
 *
 * Signature: ()
 *
 * Lowlevel C call: not necessary, you can actually pass
 * NULL as the capture.
 */
extern VROOM__Object* VROOM__STACK__Stack_new;

/* VROOM__STACK__Stack_push
 *
 * This method pushes a node to the stack. This will cause the
 * following operation:
 *
 * If the stack is empty, the given node is considered the first
 * frame.
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
 *     (VROOM__STACK__Stack $stack: VROOM__STACK__Node $node);
 *
 * Lowlevel C call:
 *     vroom__stack__stack_push_capture(VROOM__Object* stack,
 *                                     VROOM__Object* node);
 */
extern VROOM__Object* VROOM__STACK__Stack_push;
VROOM__Object* vroom__stack__stack_push_capture(VROOM__Object* stack,
                                              VROOM__Object* node);

/* VROOM__STACK__Stack_continues
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
 *     (VROOM__STACK__Stack $stack: VROOM__STACK__Node $node);
 *
 * Lowlevel C call:
 *     vroom__stack__stack_continues_capture(VROOM__Object* stack,
 *                                          VROOM__Object* node);
 * 
 */
extern VROOM__Object* VROOM__STACK__Stack_continues;
VROOM__Object* vroom__stack__stack_continues_capture(VROOM__Object* stack,
                                                   VROOM__Object* node);

/* VROOM__STACK__Stack_next
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
 * Signature:
 *     (VROOM__STACK__Stack $stack: )
 *
 * Lowlevel C call:
 *     vroom__stack__stack_next_capture(VROOM__Object* stack);
 */
extern VROOM__Object* VROOM__STACK__Stack_next;
VROOM__Object* vroom__stack__stack_next_capture(VROOM__Object* stack);

/* VROOM__STACK__Stack_current
 *
 * Returns the currently selected node.
 *
 * Signature:
 *     (VROOM__STACK__Stack $stack: )
 *
 * Lowlevel C call:
 *     vroom__stack__stack_current_capture(VROOM__Object* stack);
 */
extern VROOM__Object* VROOM__STACK__Stack_current;
VROOM__Object* vroom__stack__stack_current_capture(VROOM__Object* stack);

/* VROOM__STACK__Stack_eval
 *
 * Evaluate the selected node, and only the selected node, storing the
 * result in the node. This method won't recurse in the C stack, it
 * will, in the case this node would cause a C recursion, push
 * something to the given stack. In a very clear vision, it calls the
 * message giving it the invocant stack as the call stack. It returns
 * the return value of the node.
 *
 * Signature:
 *     (VROOM__STACK__Stack $stack: )
 * Lowlevel C call:
 *     vroom__stack__stack_eval_capture(VROOM__Object* stack);
 */
extern VROOM__Object* VROOM__STACK__Stack_eval;
VROOM__Object* vroom__stack__stack_current_capture(VROOM__Object* stack);

/* VROOM__STACK__Stack_result
 *
 * Returns the result of some past node, counting backwards. This
 * means that 1 is the immediate past node, 2 is that nodes past node
 * and so on.
 *
 * Signature:
 *     (VROOM__STACK__Stack $stack: int node)
 * Lowlevel C call:
 *     vroom__stack__stack_result_capture(VROOM__Object* stack, int node);
 */
extern VROOM__Object* VROOM__STACK__Stack_result;
VROOM__Object* vroom__stack__stack_result_capture(VROOM__Object* stack, int node);

/* VROOM__STACK__Stack_has_next
 *
 * Returns true if the stack is not empty and false if it is.
 *
 * Signature:
 *     (VROOM__STACK__Stack $stack: )
 * Lowlevel C call:
 *     vroom__stack__stack_has_next_capture(VROOM__Object* stack);
 */
extern VROOM__Object* VROOM__STACK__Stack_has_next;
VROOM__Object* vroom__stack__stack_has_next_capture(VROOM__Object* stack);

/* VROOM__STACK__Stack_loop
 *
 * This is the only method that will use the C stack. It will return
 * when the stack gets empty, it will iterate in the given stack using
 * has_hext, next and eval until the stack is empty. The return value
 * will be the return value of the last node evaled in the stack.
 *
 * Signature:
 *     (VROOM__STACK__Stack $stack: )
 * Lowlevel C call:
 *     vroom__stack__stack_loop_capture(VROOM__Object* stack);
 */
extern VROOM__Object* VROOM__STACK__Stack_loop;
VROOM__Object* vroom__stack__stack_loop_capture(VROOM__Object* stack);

/* VROOM__STACK__Node
 *
 * The node type is just a placeholder for the data operated by the
 * stack. It's a simple object that have simple accessors. These are
 * not the standard Perl accessors, are explicit methods that handle
 * it.
 *
 * One way or another, this class is closed and final.
 */
extern VROOM__Object* VROOM__STACK__Node;

/* VROOM__STACK__Node_new
 *
 * This creates a new Node object
 */
extern VROOM__Object* VROOM__STACK__Node_new;
extern VROOM__Object* VROOM__STACK__Node_metaclass;
extern VROOM__Object* VROOM__STACK__Node_identifier;
extern VROOM__Object* VROOM__STACK__Node_capture;
extern VROOM__Object* VROOM__STACK__Node_debug;
extern VROOM__Object* VROOM__STACK__Node_jail;
extern VROOM__Object* VROOM__STACK__Node_lexical;
extern VROOM__Object* VROOM__STACK__Node_outer;
extern VROOM__Object* VROOM__STACK__Node_continuation;
extern VROOM__Object* VROOM__STACK__Node_past;
extern VROOM__Object* VROOM__STACK__Node_result;


#endif
