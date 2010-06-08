#ifndef SMOP_S0NATIVE_H
#include <smop/base.h>
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

extern SMOP__Object* SMOP__metaRI;

extern SMOP__ResponderInterface* SMOP__NATIVE__idconst_RI;

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
SMOP__Object* SMOP__NATIVE__idconst_create(const char* value);

char* SMOP__NATIVE__idconst_fetch(SMOP__Object* value, int* retsize);
char* SMOP__NATIVE__idconst_fetch_with_null(SMOP__Object* value,int* retsize);

SMOP__Object* smop_noop_weakref(SMOP__Object* interpreter,
                                  SMOP__ResponderInterface* responder,
                                  SMOP__Object* obj); 

void smop_metaRI_init();
void smop_metaRI_destr();

#ifdef SMOP_PROFILE
#include <stdio.h>
extern int SMOP_PROFILE_on;
FILE* SMOP_PROFILE_out;
void smop_profile_init(void);
#endif

#endif

