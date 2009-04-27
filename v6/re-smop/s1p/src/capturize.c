#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <smop/s1p.h>
#include <smop/base.h>
#include <smop/s0native.h>

static SMOP__Object* SMOP__ID__capturize;
SMOP__Object* SMOP__S1P__Capturize;
static SMOP__Object* capturize_message(SMOP__Object* interpreter,
                                       SMOP__ResponderInterface* self,
                                       SMOP__Object* identifier,
                                       SMOP__Object* capture) {
  if (identifier == SMOP__ID__capturize) {
    return capture;
  }

  SMOP_RELEASE(interpreter, capture);

  return SMOP__NATIVE__bool_false;
}

static SMOP__Object* noop(SMOP__Object* interpreter,
                          SMOP__ResponderInterface* responder,
                          SMOP__Object* obj) {
  return obj;
}

void smop_s1p_capturize_init(SMOP__Object* interpreter) {

  SMOP__ID__capturize = SMOP__NATIVE__idconst_create("capturize");
  SMOP__S1P__Capturize = calloc(1, sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__Capturize)->RI = SMOP__S1P__Capturize;
  ((SMOP__ResponderInterface*)SMOP__S1P__Capturize)->MESSAGE = capturize_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Capturize)->REFERENCE = noop;
  ((SMOP__ResponderInterface*)SMOP__S1P__Capturize)->RELEASE = noop;
  ((SMOP__ResponderInterface*)SMOP__S1P__Capturize)->WEAKREF = noop;
  ((SMOP__ResponderInterface*)SMOP__S1P__Capturize)->id = "S1P Capture";

}

void smop_s1p_capturize_destr(SMOP__Object* interpreter) {
  free(SMOP__S1P__Capturize);
}
