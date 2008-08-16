#include <stdlib.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>
#include <stdio.h>

SMOP__Object* SMOP__NATIVE__int;

typedef struct smop_native_int_struct {
  SMOP__Object__BASE
  int intvalue;
} smop_native_int_struct;

static SMOP__Object* int_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {

  ___CONST_IDENTIFIER_ONLY___;

  SMOP__Object* ret = SMOP__NATIVE__bool_false;

  if (identifier == SMOP__ID__infix_num_gt) {

    ___NATIVE_CAPTURE_ONLY___;
    ___INVOCANT_RI_SHOULD_MATCH___;

    SMOP__Object* other = SMOP__NATIVE__capture_positional(interpreter,capture,0);
    if (SMOP_RI(other) == (SMOP__ResponderInterface*)SMOP__NATIVE__int) {
      //fprintf(stderr,"native int: %d > %d\n", SMOP__NATIVE__int_fetch(invocant),
      //        SMOP__NATIVE__int_fetch(other));
      if (SMOP__NATIVE__int_fetch(invocant) > SMOP__NATIVE__int_fetch(other)) {
        ret = SMOP__NATIVE__bool_true;
      } else {
        ret = SMOP__NATIVE__bool_false;
      }
    } else {
      ___UNKNOWN_METHOD___;
    }

    SMOP_RELEASE(interpreter, other);
    SMOP_RELEASE(interpreter,invocant);

  } else if (identifier == SMOP__ID__DESTROYALL) {
    // notthing
  } else {
    ___UNKNOWN_METHOD___;
  }

  SMOP_RELEASE(interpreter,capture);
  return ret;
}


void smop_native_int_init() {
  SMOP__NATIVE__int = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__NATIVE__int)->MESSAGE = int_message;
  ((SMOP__ResponderInterface*)SMOP__NATIVE__int)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__NATIVE__int)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__NATIVE__int)->id = "Native int";
}

void smop_native_int_destr() {
  free(SMOP__NATIVE__int);
}

SMOP__Object* SMOP__NATIVE__int_create(int value) {
  SMOP__Object* ret = smop_lowlevel_alloc(sizeof(smop_native_int_struct));
  ret->RI = (SMOP__ResponderInterface*)SMOP__NATIVE__int;
  ((smop_native_int_struct*)ret)->intvalue = value;
  return ret;
}

int SMOP__NATIVE__int_fetch(SMOP__Object* value) {
  smop_lowlevel_rdlock(value);
  int v = ((smop_native_int_struct*)value)->intvalue;
  smop_lowlevel_unlock(value);
  return v;
}
