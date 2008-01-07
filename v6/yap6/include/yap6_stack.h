
#ifndef YAP6_STACK_H
#define YAP6_STACK_H

#include <yap6_base.h> // this is declared by yap6.h which is the one
                       // who includes this file, but let's keep this
                       // here if anyone wants to include only parts
                       // of yap6.h. It should do no harm because of
                       // the ifndefs of the beggining of the file.

// this is the metaclass proto-object that implements the
// lowlevel operations that deal with the current stack.
extern YAP6__Prototype* YAP6__STACK__Operators;
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
