
#ifndef SMOP_SLIME_H
#define SMOP_SLIME_H

#include <smop.h>


/* Please see
http://www.perlfoundation.org/perl6/index.cgi?default_smop_interpreter_implementation
*/
extern SMOP__Object* SMOP__SLIME__Stack;
extern SMOP__Object* SMOP__SLIME__Node;


/* SMOP__SLIME__Operators
 *
 * This is the responder interface that support all slime operators as
 * described below.
 *
 * This object is closed and final.
 */
extern SMOP__Object* SMOP__SLIME__Operators;

/* The following 5 operators will allways manipulate the current
 * continuation. It is strongly advised that this should only be
 * called by the interpreter loop itself, with a running frame. They
 * are exposed to the high-level space so you can build a new frame
 * referencing them. Calling this directly will probably cause a
 * segfault.  This operators are:
 *
 * forget
 * move_capturize
 * move_identifier
 * move_responder
 * copy
 *
 */

/* forget
 *
 * This operator simply drop the past nodes of the current frame and
 * let them be destroyed. Can be used by optimizers when return values
 * are no longer needed in a frame, causing non-lexical values to be
 * destroyed even before the end of the block.
 *
 * This operator does not receive any parameter. It have an empty
 * signature and may receive a NULL capture.
 */

/* move_capturize
 *
 * Creates a low-level capture from previous frame nodes and store it
 * in a continuation node, supporting the invocant, positional and
 * named arguments. This operator operates in the current frame and
 * calling it directly from the high level seems very
 * innapropriate. The high level is supposed only to use it to define
 * nodes that will be evaluated later. This code will remove the
 * result of the referenced nodes in order to avoid refcount
 * manipulation. If you want to use a result from one node in more
 * than one capture you should use the copy operator.
 *
 * lowlevel C call: smop__slime__opcapture_move_capturize_new
 *
 * This subroutine receives a C int pointing to which past node should
 * the invocant be taken, two C int null-terminated arrays pointing
 * the past nodes that contain each of the positional arguments and
 * each of the pairs that compose the named arguments. This is not the
 * operator call yet, it just creates the capture that can be stored
 * in a frame node for a later call.
 *
 * highlevel capture prototype: SMOP__SLIME__OPCAPTURE_Move_Capturize
 *
 * This is the prototype that will be able to create this capture
 * object in the high level. The "new" signature to create this should
 * be (int, List of int, List of int, int). But again, this is just
 * the capture creator for the operator and not the operator itself.
 */
SMOP__Object* smop__slime__opcapture_move_capturize_new(int invocant,
                                                          int* positional,
                                                          int* named,
                                                          int target);
extern SMOP__Object* SMOP__SLIME__OPCAPTURE_Move_Capturize;

/* move_identifier
 *
 * Move the result of a given past node to a target node as the
 * identifier of the message. This operator shouldn't probably be used
 * directly from the high-level as it manipulates the current frame,
 * being referenced in new nodes for later evaluation only. This
 * operator moves the result, if you want to use a result more than
 * once, use the copy operator.
 *
 * lowlevel C call: smop__slime__op_move_identifier_new
 *
 * This subroutine receives two C ints and creates an object that can
 * be used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: SMOP__SLIME__OPCAPTURE_Move_Identifier
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will be (int,
 * int).
 */
SMOP__Object* smop__slime__opcapture_move_identifier_new(int source, int target);
extern SMOP__Object* SMOP__SLIME__OPCAPTURE_Move_Identifier;

/* move_responder
 *
 * Move the result of a given past node to a target node as the
 * responder of the message. This operator shouldn't probably be used
 * directly from the high-level as it operates on the current frame,
 * being only referenced in nodes for future evaluation. This operator
 * moves the result, if you want to use a result more than once, use
 * the copy operator.
 *
 * lowlevel C call: smop__slime__opcapture_move_responder_new
 *
 * This subroutine receives two C ints and creates an object that can
 * be used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: SMOP__SLIME__OPCAPTURE_Move_Responder
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will be (int,
 * int).
 */
SMOP__Object* smop__slime__opcapture_move_responder_new(int source, int target);
extern SMOP__Object* SMOP__SLIME__OPCAPTURE_Move_Responder;

/* copy
 *
 * This operator copies the result of some past node and returns
 * it. This is usefull when you want the result of some node to be
 * available to more than one of the move operators. This operator
 * manipulates the current frame.
 *
 * lowlevel C call: smop__slime__opcapture_copy_new
 *
 * This subroutine receives a C int and create an object that can be
 * used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: SMOP__SLIME__OPCAPTURE_Copy
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will simply be
 * (int).
 */
SMOP__Object* smop__slime__opcapture_copy_new(int source);
extern SMOP__Object* SMOP__SLIME__OPCAPTURE_Copy;


#endif
