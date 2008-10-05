#include <smop.h>
#include <smop_s1p.h>
#include "internal.h"
#include <stdio.h>

SMOP__Object* SMOP__GlobalInterpreter;
void smop_init() {
  SMOP_INTERNAL_BOOT_SEQUENCE;
  SMOP_INTERNAL_INIT_SEQUENCE;

  SMOP__GlobalInterpreter = SMOP_DISPATCH(SMOP__INTPTR__InterpreterInstance, SMOP_RI(SMOP__INTPTR__InterpreterInstance),
                                      SMOP__ID__new, 
                                      SMOP__NATIVE__capture_create(SMOP__INTPTR__InterpreterInstance,
                                                                   SMOP__INTPTR__InterpreterInstance,NULL,NULL));

  SMOP_BOOTSTRAP_INIT_SEQUENCE;
}

void smop_destr() {

  SMOP_BOOTSTRAP_DESTR_SEQUENCE;

  SMOP_DISPATCH(SMOP__GlobalInterpreter, SMOP_RI(SMOP__GlobalInterpreter),
                SMOP__ID__loop, 
                SMOP__NATIVE__capture_create(SMOP__GlobalInterpreter,
                                             SMOP_REFERENCE(SMOP__GlobalInterpreter,SMOP__GlobalInterpreter),
                                             NULL, NULL));

  SMOP_DISPATCH(SMOP__GlobalInterpreter, SMOP_RI(SMOP__GlobalInterpreter),
                SMOP__ID__goto, 
                SMOP__NATIVE__bool_false);

  SMOP_DISPATCH(SMOP__GlobalInterpreter, SMOP_RI(SMOP__GlobalInterpreter),
                SMOP__ID__loop, 
                SMOP__NATIVE__capture_create(SMOP__GlobalInterpreter,
                                             SMOP_REFERENCE(SMOP__GlobalInterpreter,SMOP__GlobalInterpreter),
                                             NULL, NULL));

  SMOP_INTERNAL_DESTROY_SEQUENCE;
  SMOP_RELEASE(SMOP__INTPTR__InterpreterInstance,SMOP__GlobalInterpreter);
  SMOP_INTERNAL_SHUTDOWN_SEQUENCE;
}
