#include <smop.h>
#include <smop_slime.h>

SMOP__Object* SMOP__SLIME__CurrentFrame;

static SMOP__Object* currentframe_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  SMOP__Object* frame = SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                                      SMOP__ID__continuation,
                                      SMOP__NATIVE__capture_create(interpreter,NULL,NULL,NULL));
  SMOP__Object* delegated = SMOP__NATIVE__capture_delegate(interpreter,
                                                           frame,
                                                           capture);
  return SMOP_DISAPTCH(interpreter, SMOP_RI(frame),
                       identifier, delegated);
}

static SMOP__Object* currentframe_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  return obj;
}

static SMOP__Object* currentframe_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  return obj;
}


void smop_slime_currentframe_init() {
  SMOP__SLIME__CurrentFrame = calloc(sizeof(SMOP__ResponderInterface));
  SMOP__SLIME__CurrentFrame->MESSAGE = currentframe_message;
  SMOP__SLIME__CurrentFrame->REFERENCE = currentframe_reference;
  SMOP__SLIME__CurrentFrame->RELEASE = currentframe_release;
}

void smop_slime_currentframe_destr() {
  free(SMOP__SLIME__CurrentFrame);
}
