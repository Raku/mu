
#ifndef YAP6_STACK_H
#define YAP6_STACK_H

#include <yap6_base.h> // this is declared by yap6.h which is the one
                       // who includes this file, but let's keep this
                       // here if anyone wants to include only parts
                       // of yap6.h. It should do no harm because of
                       // the ifndefs of the beggining of the file.

/*
 * In YAP6, every low-level opeator is actually a method call in the
 * metaclass of the lowlevel module. For the stack, the
 * YAP6__STACK__Operators prototype is actually also a metaclass that
 * handles special methods. Calling it from the low-level works the
 * same as calling from the high level. Every operation message is
 * composed by basically three elements:
 *
 *   metaclass: which metaclass handles this message
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
 * specifc objects don't use the YAP6 stack, only the C stack which
 * makes you free to call them anytime, by using the lowlevel C
 * subroutine, while the prototype will still support a high-level
 * call that builds the same object.
 *
 */


/* YAP6__STACK__Operators
 *
 * This is the metaclass that support all stack operators as described
 * below.
 */
extern YAP6__Prototype* YAP6__STACK__Operators;


/* YAP6__STACK__OP_Move_Capturize
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
 * lowlevel C call: yap6__stack__opcapture_move_capturize_new
 *
 * This subroutine receives a C int pointing to which past node should
 * the invocant be taken, two C int null-terminated arrays pointing
 * the past nodes that contain each of the positional arguments and
 * each of the pairs that compose the named arguments. This is not the
 * operator call yet, it just creates the capture that can be stored
 * in a stack node for a later call.
 *
 * highlevel capture prototype: YAP6__STACK__OPCAPTURE_Move_Capturize
 *
 * This is the prototype that will be able to create this capture
 * object in the high level. The "new" signature to create this should
 * be (int, List of int, List of int, int). But again, this is just
 * the capture creator for the operator and not the operator itself.
 */
YAP6__Object* yap6__stack__opcapture_move_capturize_new(int invocant,
                                                        int** positional,
                                                        int** named,
                                                        int target);
extern YAP6__Prototype* YAP6__STACK__OPCAPTURE_Move_Capturize;
extern YAP6__Object* YAP6__STACK__OP_Move_Capturize;

/* YAP6__STACK__OP_Move_Identifier
 *
 * Move the result of a given past node to a target node as the
 * identifier of the message. This operator shouldn't probably be used
 * directly from the high-level as it manipulates the current stack,
 * being referenced in new nodes for later evaluation only. This
 * operator moves the result, if you want to use a result more than
 * once, use the copy operator.
 *
 * lowlevel C call: yap6__stack__op_move_identifier_new
 *
 * This subroutine receives two C ints and creates an object that can
 * be used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: YAP6__STACK__OPCAPTURE_Move_Identifier
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will be (int,
 * int).
 */
YAP6__Object* yap6__stack__opcapture_move_identifier_new(int source, int target);
extern YAP6__Prototype* YAP6__STACK__OPCAPTURE_Move_Identifier;
extern YAP6__Object* YAP6__STACK__OP_Move_Identifier;

/* YAP6__STACK__OP_Move_MetaClass
 *
 * Move the result of a given past node to a target node as the
 * metaclass of the message. This operator shouldn't probably be used
 * directly from the high-level as it operates on the current stack,
 * being only referenced in nodes for future evaluation. This operator
 * moves the result, if you want to use a result more than once, use
 * the copy operator.
 *
 * lowlevel C call: yap6__stack__opcapture_move_metaclass_new
 *
 * This subroutine receives two C ints and creates an object that can
 * be used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: YAP6__STACK__OPCAPTURE_Move_MetaClass
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will be (int,
 * int).
 */
YAP6__Object* yap6__stack__opcapture_move_metaclass_new(int source, int target);
extern YAP6__Prototype* YAP6__STACK__OPCAPTURE_Move_MetaClass;
extern YAP6__Object* YAP6__STACK__OP_Move_MetaClass;

/* YAP6__STACK__OP_Copy
 *
 * This operator copies the result of some past node to the result of
 * this node. This is usefull when you want the result of some node to
 * be available to more than one of the move operators.
 *
 * lowlevel C call: yap6__stack__opcapture_copy_new
 *
 * This subroutine receives a C int and create an object that can be
 * used as a capture for the above operator. It doesn't call the
 * operator itself.
 *
 * highlevel capture prototype: YAP6__STACK__OPCAPTURE_Copy
 *
 * This is the prototype that will be able to create an object
 * compatible with this operator. The "new" signature will simply be
 * (int).
 */
YAP6__Object* yap6__stack__opcapture_copy_new(int source);
extern YAP6__Prototype* YAP6__STACK__OPCAPTURE_Copy;
extern YAP6__Object* YAP6__STACK__OP_Copy;


extern YAP6__Object* YAP6__STACK__OP_Stack_Init;
extern YAP6__Object* YAP6__STACK__OP_Stack_Push;
extern YAP6__Object* YAP6__STACK__OP_Stack_Continues;
extern YAP6__Object* YAP6__STACK__OP_Stack_Next;
extern YAP6__Object* YAP6__STACK__OP_Stack_Current;
extern YAP6__Object* YAP6__STACK__OP_Stack_Eval;
extern YAP6__Object* YAP6__STACK__OP_Stack_Result;
extern YAP6__Object* YAP6__STACK__OP_Stack_IsEmpty;
extern YAP6__Object* YAP6__STACK__OP_Stack_Loop;


extern YAP6__Object* YAP6__STACK__OP_Node_Init;
extern YAP6__Object* YAP6__STACK__OP_Node_MetaClass;
extern YAP6__Object* YAP6__STACK__OP_Node_Identifier;
extern YAP6__Object* YAP6__STACK__OP_Node_Capture;
extern YAP6__Object* YAP6__STACK__OP_Node_Debug;
extern YAP6__Object* YAP6__STACK__OP_Node_Jail;
extern YAP6__Object* YAP6__STACK__OP_Node_Lexical;
extern YAP6__Object* YAP6__STACK__OP_Node_Outer;
extern YAP6__Object* YAP6__STACK__OP_Node_Continuation;
extern YAP6__Object* YAP6__STACK__OP_Node_Past;
extern YAP6__Object* YAP6__STACK__OP_Node_Result;

// And the Stack and Node prototypes.
extern YAP6__Prototype* YAP6__STACK__Stack;
extern YAP6__Prototype* YAP6__STACK__Node;

YAP6__Object* YAP6__STACK__Stack_Init();
YAP6__Object* YAP6__STACK__Stack_Push(YAP6__Object* stack, YAP6__Object* node);
YAP6__Object* YAP6__STACK__Stack_Continue(YAP6__Object* stack, YAP6__Object* node);
YAP6__Object* YAP6__STACK__Stack_Drop(YAP6__Object* stack);
YAP6__Object* YAP6__STACK__Stack_Next(YAP6__Object* stack);
YAP6__Object* YAP6__STACK__Stack_Curr(YAP6__Object* stack);
YAP6__Object* YAP6__STACK__Stack_Eval(YAP6__Object* stack);
YAP6__Object* YAP6__STACK__Stack_Result(YAP6__Object* stack, int count_backwards);
YAP6__Object* YAP6__STACK__Stack_IsEmpty(YAP6__Object* stack);

YAP6__Object* YAP6__STACK__Node_Init();
// All getters are setters at the same time. If you want to use only as getter, pass NULL.
YAP6__Object* YAP6__STACK__Node_MetaClass(YAP6__Object* node, YAP6__Object* newvalue);
YAP6__Object* YAP6__STACK__Node_Identifier(YAP6__Object* node,  YAP6__Object* newvalue);
YAP6__Object* YAP6__STACK__Node_Capture(YAP6__Object* node, YAP6__Object* newvalue);
YAP6__Object* YAP6__STACK__Node_Debug(YAP6__Object* node, YAP6__Object* newvalue);
YAP6__Object* YAP6__STACK__Node_Jail(YAP6__Object* node, YAP6__Object* newvalue);
YAP6__Object* YAP6__STACK__Node_Lexical(YAP6__Object* node, YAP6__Object* newvalue);
YAP6__Object* YAP6__STACK__Node_Outer(YAP6__Object* node, YAP6__Object* newvalue);
YAP6__Object* YAP6__STACK__Node_Continuation(YAP6__Object* node, YAP6__Object* newvalue);
YAP6__Object* YAP6__STACK__Node_Past(YAP6__Object* node, YAP6__Object* newvalue);
YAP6__Object* YAP6__STACK__Node_Result(YAP6__Object* node, YAP6__Object* newvalue);

#endif
