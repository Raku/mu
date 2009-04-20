#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/lost.h>

static SMOP__NAGC__ResponderInterface RI;

SMOP__Object* frame_message(SMOP__Object* interpreter,
                            SMOP__ResponderInterface* self,
                            SMOP__Object* identifier,
                            SMOP__Object* capture) {
  
}

SMOP__Object* SMOP__LOST__Frame_create(SMOP__Object* interpreter,
                                       SMOP__Object* back,
                                       void* user,
                                       int (step*)(SMOP__Object* interpreter,
                                                   SMOP__Object* frame),
                                       int (destr*)(SMOP__Object* interpreter,
                                                    SMOP__Object* frame)) {
  SMOP__Object* frame = smop_nagc_alloc(sizeof(SMOP_LOST_Frame));
  frame->RI = &RI;
  frame->back = back;
  frame->user = user;
  frame->pc = 0;
  frame->lastr = SMOP__NATIVE__bool_false;
  frame->step = step;
  frame->destr = destr;
  return frame;
}

void smop_lost_init() {
  RI->MESSAGE = frame_message;
  RI->REFERENCE = smop_nagc_reference;
  RI->RELEASE = smop_nagc_release;
  RI->WEAKREF = smop_nagc_weakref;
  RI->id = "lost frame";
}

void smop_lost_destr() {

}
