#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>

SMOP__Object* SMOP__S1P__Capturize;

static SMOP__Object* capturize_message(SMOP__Object* interpreter,
                                       SMOP__ResponderInterface* self,
                                       SMOP__Object* identifier,
                                       SMOP__Object* capture) {

  ___CONST_IDENTIFIER_ONLY___;

  SMOP__Object* ret;
  
  if (identifier == SMOP__ID__capturize) {
    return capture;
  } else {
    ___UNKNOWN_METHOD___;
  }

  SMOP_RELEASE(interpreter, capture);

  return ret;
}

void smop_s1p_capturize_init() {

  SMOP__S1P__Capturize = calloc(1, sizeof(SMOP__ResponderInterface));
  assert(SMOP__S1P__Capturize);
  ((SMOP__ResponderInterface*)SMOP__S1P__Capturize)->MESSAGE = capturize_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Capturize)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Capturize)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Capturize)->id = "S1P Capture";

}

void smop_s1p_capturize_destr() {

  free(SMOP__S1P__Capturize);

}
