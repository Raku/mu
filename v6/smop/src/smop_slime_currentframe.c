#include <stdio.h>
#include <stdlib.h>
#include <smop.h>
#include <smop_slime.h>

SMOP__Object* SMOP__SLIME__CurrentFrame;

static SMOP__Object* currentframe_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {

  SMOP__Object* frame = SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                                      SMOP__ID__continuation,
                                      SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,interpreter),NULL,NULL));
  SMOP__Object* delegated;
  if (SMOP_RI(capture) == (SMOP__ResponderInterface*)SMOP__NATIVE__capture) {
    delegated = SMOP__NATIVE__capture_delegate(interpreter,
                                               frame,
                                               capture);
  } else {
    delegated = capture;
  }
  
  if (frame) {
    return SMOP_DISPATCH(interpreter, SMOP_RI(frame),
                         identifier, delegated);
  } else {
    fprintf(stderr, "[SMOP__SLIME__CurrentFrame] Cannot DISPATCH method to current frame without a frame.\n");
    return NULL;
  }
}

static SMOP__Object* currentframe_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  return obj;
}

static SMOP__Object* currentframe_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  return obj;
}


void smop_slime_currentframe_init() {
  SMOP__SLIME__CurrentFrame = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__SLIME__CurrentFrame)->MESSAGE = currentframe_message;
  ((SMOP__ResponderInterface*)SMOP__SLIME__CurrentFrame)->REFERENCE = currentframe_reference;
  ((SMOP__ResponderInterface*)SMOP__SLIME__CurrentFrame)->RELEASE = currentframe_release;
  ((SMOP__ResponderInterface*)SMOP__SLIME__CurrentFrame)->id = "SMOP SLIME CurrentFrame";
}

void smop_slime_currentframe_destr() {
  free(SMOP__SLIME__CurrentFrame);
}
