#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <smop.h>
#include <smop_s1p.h>
#include <smop_native.h>

SMOP__Object* SMOP__NATIVE__bool;
SMOP__Object* SMOP__NATIVE__bool_true;
SMOP__Object* SMOP__NATIVE__bool_false;

static SMOP__Object* bool_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {

  ___NATIVE_CAPTURE_ONLY___;
  ___INVOCANT_RI_SHOULD_MATCH___;
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  if (identifier == SMOP__ID__bool) {
    ret = SMOP_REFERENCE(interpreter,invocant);
  } else if (identifier == SMOP__ID__defined) {
    ret = SMOP__NATIVE__bool_true;

  } else if (identifier == SMOP__ID__FETCH) {
    ___VALUE_FETCH___;

  } else if (identifier == SMOP__ID__STORE) {
    ___VALUE_STORE___;

  } else {
    ___UNKNOWN_METHOD___;

  }

  SMOP_RELEASE(interpreter,invocant);
  SMOP_RELEASE(interpreter,capture);
  return ret;
}

static SMOP__Object* bool_reference(SMOP__Object* interpreter,
                                    SMOP__ResponderInterface* responder,
                                    SMOP__Object* obj) {
  return obj;
}

static SMOP__Object* bool_release(SMOP__Object* interpreter,
                                  SMOP__ResponderInterface* responder,
                                  SMOP__Object* obj) {
  return obj;
}

void smop_native_bool_init() {

  SMOP__NATIVE__bool = calloc(1,sizeof(SMOP__ResponderInterface));
  SMOP__ResponderInterface* ri = (SMOP__ResponderInterface*)SMOP__NATIVE__bool;
  assert(SMOP__NATIVE__bool);
  ri->MESSAGE = bool_message;
  ri->REFERENCE = bool_reference;
  ri->RELEASE = bool_release;
  ri->id = "Native Boolean";

  SMOP__NATIVE__bool_true = calloc(1,sizeof(SMOP__Object));
  assert(SMOP__NATIVE__bool_true);
  SMOP__NATIVE__bool_true->RI = ri;
  SMOP__NATIVE__bool_true->data = (void*)1;

  SMOP__NATIVE__bool_false = calloc(1,sizeof(SMOP__Object));
  assert(SMOP__NATIVE__bool_false);
  SMOP__NATIVE__bool_false->RI = ri;
  SMOP__NATIVE__bool_false->data = NULL;
  
}

void smop_native_bool_destr() {
  free(SMOP__NATIVE__bool_false);
  free(SMOP__NATIVE__bool_true);
  free(SMOP__NATIVE__bool);
}


SMOP__Object* SMOP__NATIVE__bool_create(int b) {
  if (b) {
    return SMOP__NATIVE__bool_true;
  } else {
    return SMOP__NATIVE__bool_false;
  }
}

int SMOP__NATIVE__bool_fetch(SMOP__Object* v) {
  if (v->data) {
    return 1;
  } else {
    return 0;
  }
}
