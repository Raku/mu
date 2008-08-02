#include <stdlib.h>
#include <stdio.h>
#include <smop.h>
#include <string.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>

SMOP__Object* SMOP__Mold;

typedef struct smop_mold {
  SMOP__Object__BASE
  int registers;
  SMOP__Object** constants;
  int *opcodes;
} smop_mold;

SMOP__Object* SMOP__Mold_create(int registers,SMOP__Object** constants,int opcodes_len,int *opcodes) {
    smop_mold* ret = (smop_mold*) smop_lowlevel_alloc(sizeof(smop_mold));
    ret->RI = (SMOP__ResponderInterface*)SMOP__Mold;
    ret->registers = registers;

    int i;
    for (i = 0;constants[i];i++);
    i++;

    ret->constants = malloc(sizeof(SMOP__Object*) * i);
    memcpy(ret->constants,constants,sizeof(SMOP__Object*) * i);

    ret->opcodes = malloc(sizeof(int) * opcodes_len);
    memcpy(ret->opcodes,opcodes,sizeof(int) * opcodes_len);

    return (SMOP__Object*) ret;
}

static SMOP__Object* smop_mold_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  ___NATIVE_CAPTURE_ONLY___;
  ___CONST_IDENTIFIER_ONLY___;
  ___INVOCANT_RI_SHOULD_MATCH___;

  smop_mold* mold = (smop_mold*)invocant;
  if (SMOP__ID__DESTROYALL == identifier) {
    int i;
    for (i=0;mold->constants[i];i++) {
      SMOP_RELEASE(interpreter,mold->constants[i]);
    }
    free(mold->constants);
    free(mold->opcodes);
  }
  if (invocant) SMOP_RELEASE(interpreter,invocant);

  SMOP_RELEASE(interpreter,capture);
  return SMOP__NATIVE__bool_false;
}


void smop_mold_init() {
  SMOP__Mold = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__Mold)->MESSAGE = smop_mold_message;
  ((SMOP__ResponderInterface*)SMOP__Mold)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__Mold)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__Mold)->id = "mold";
}

void smop_mold_destr() {
  free(SMOP__Mold);
}
