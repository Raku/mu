#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/native.h>
#include <smop/lost.h>
#include <stdlib.h>

static SMOP__NAGC__ResponderInterface* RI;

static SMOP__Object* MESSAGE(SMOP__Object* interpreter,
                            SMOP__ResponderInterface* self,
                            SMOP__Object* identifier,
                            SMOP__Object* capture) {
  
}
static void DESTROYALL(SMOP__Object* interpreter,SMOP__Object* value) {
  
}

SMOP__Object* SMOP__LOST__Frame_create(SMOP__Object* interpreter,
                                       SMOP__Object* back,
                                       void* user,
                                       int (*step)(SMOP__Object* interpreter,
                                                   SMOP__Object* frame),
                                       void (*destr)(SMOP__Object* interpreter,
                                                    SMOP__Object* frame)) {
  SMOP_LOST_Frame* frame = (SMOP_LOST_Frame*) smop_nagc_alloc(sizeof(SMOP_LOST_Frame));
  frame->RI = RI;
  frame->back = back;
  frame->user = user;
  frame->pc = 0;
  frame->lastr = SMOP__NATIVE__bool_false;
  frame->step = step;
  frame->destr = destr;
  return frame;
}

void smop_lost_init() {
  RI = SMOP__NAGC__RI__create(MESSAGE,smop_nagc_reference,smop_nagc_release,smop_nagc_weakref,DESTROYALL,"lost frame");
}

void smop_lost_destr() {
  free(RI);
}
