#ifndef SMOP_S0NATIVE_H
#include <smop_base.h>
#define SMOP_S0NATIVE_H
void smop_s0native_init(void);
void smop_s0native_destr(void);

void smop_idconst_init(void);
void smop_idconst_destr(void);

void smop_native_bool_init(void);
void smop_native_bool_destr(void);

void smop_empty_interpreter_init(void);
void smop_empty_interpreter_destr(void);

extern SMOP__Object* SMOP__NATIVE__bool_true;
extern SMOP__Object* SMOP__NATIVE__bool_false;
extern SMOP__Object* SMOP__EmptyInterpreter;

SMOP__Object* smop_placeholder_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture);


SMOP__Object* smop_noop_reference(SMOP__Object* interpreter,
                                    SMOP__ResponderInterface* responder,
                                    SMOP__Object* obj);

SMOP__Object* smop_noop_release(SMOP__Object* interpreter,
                                  SMOP__ResponderInterface* responder,
                                  SMOP__Object* obj); 
SMOP__Object* SMOP__NATIVE__idconst_createn(const char* value, int size);

SMOP__Object* smop_noop_weakref(SMOP__Object* interpreter,
                                  SMOP__ResponderInterface* responder,
                                  SMOP__Object* obj); 
#endif
