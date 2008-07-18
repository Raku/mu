#include <smop.h>
#include <smop_s1p.h>
#include "smop_internal.h"
#include <stdio.h>

SMOP__Object* SMOP__GlobalInterpreter;
void smop_init() {
  SMOP_INTERNAL_BOOT_SEQUENCE;
  SMOP_INTERNAL_INIT_SEQUENCE;
  printf("creating interpreter\n");
  SMOP__GlobalInterpreter = SMOP_DISPATCH(SMOP__INTPTR__InterpreterInstance, SMOP_RI(SMOP__INTPTR__InterpreterInstance),
                                      SMOP__ID__new, 
                                      SMOP__NATIVE__capture_create(SMOP__INTPTR__InterpreterInstance,
                                                                   SMOP__INTPTR__InterpreterInstance,NULL,NULL));
}

void smop_destr() {
  SMOP_INTERNAL_DESTROY_SEQUENCE;
  //SMOP_RELEASE(SMOP__INTPTR__InterpreterInstance,SMOP__GlobalInterpreter);
  SMOP_INTERNAL_SHUTDOWN_SEQUENCE;
}
