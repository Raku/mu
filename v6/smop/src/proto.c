#include <stdlib.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>
#include <stdio.h>

static SMOP__Object* RI;

typedef struct smop_proto_struct {
  SMOP__Object__BASE
  SMOP__Object* delegate_to_RI;
} smop_proto_struct;

static SMOP__Object* message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {

  ___NATIVE_CAPTURE_ONLY___;
  ___CONST_IDENTIFIER_ONLY___;
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  smop_proto_struct* invocant = (smop_proto_struct*) SMOP__NATIVE__capture_invocant(interpreter, capture);
  
  if (identifier == SMOP__ID__new) {
    ret = SMOP_DISPATCH(interpreter,invocant->delegate_to_RI,identifier,SMOP_REFERENCE(interpreter,capture));

  } else if (identifier == SMOP__ID__DESTROYALL) {

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

static SMOP__Object* ri_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  return obj;
}

static SMOP__Object* ri_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  return obj;
}

void smop_proto_init() {
  RI = SMOP__RI__create(
      message,
      ri_reference,
      ri_release,
      "proto");
}

void smop_proto_destr() {
  free(RI);
}

SMOP__Object* SMOP__Proto__create(SMOP__Object* delegate_to_RI) {
 SMOP__Object* ret = malloc(sizeof(smop_proto_struct));
 ((smop_proto_struct*)ret)->delegate_to_RI = delegate_to_RI;
 ret->RI = RI;
 return ret;
}
