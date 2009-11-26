#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <smop/base.h>
#include <smop/s0native.h>

SMOP__Object*  SMOP__EmptyInterpreter;


void smop_empty_interpreter_init() {

  SMOP__ResponderInterface* ri = (SMOP__ResponderInterface*)calloc(1,sizeof(SMOP__ResponderInterface));

  ri->MESSAGE = smop_placeholder_message;
  ri->REFERENCE = smop_noop_reference;
  ri->RELEASE = smop_noop_release;
  ri->id = "Empty Interpreter";
  ri->RI = (SMOP__ResponderInterface *)SMOP__metaRI;

  SMOP__EmptyInterpreter = calloc(1,sizeof(SMOP__Object));
  SMOP__EmptyInterpreter->RI = ri;
}

void smop_empty_interpreter_destr() {
  free(SMOP__EmptyInterpreter->RI);
  free(SMOP__EmptyInterpreter);
}
