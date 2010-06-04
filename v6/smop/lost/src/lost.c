#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/native.h>
#include <smop/lost.h>
#include <smop/capture.h>
#include <stdlib.h>

static SMOP__NAGC__ResponderInterface* RI;

static SMOP__Object* SMOP__ID__eval;
static SMOP__Object* SMOP__ID__goto;

static SMOP__Object* MESSAGE(SMOP__Object* interpreter,
                            SMOP__ResponderInterface* self,
                            SMOP__Object* identifier,
                            SMOP__Object* capture) {
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  SMOP__Object* invocant = SMOP__NATIVE__capture_positional(interpreter,capture,0);
  if (SMOP__ID__eval == identifier) {
     smop_nagc_rdlock((SMOP__NAGC__Object*)invocant);


     int (*step)(SMOP__Object* interpreter,
                 SMOP__Object* frame);
     step = ((SMOP__LOST__Frame*)invocant)->step;
     smop_nagc_unlock((SMOP__NAGC__Object*)invocant);
     int flag = step(interpreter,invocant);
     if (!flag) {
       SMOP__Object* back = ((SMOP__LOST__Frame*)invocant)->back;
       ((SMOP__LOST__Frame*)invocant)->back = NULL;
       SMOP_DISPATCH(interpreter, SMOP_RI(interpreter), SMOP__ID__goto,SMOP__NATIVE__capture_create(interpreter,(SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter), back, NULL}, (SMOP__Object*[]) {NULL}));
     }
     ret = SMOP__NATIVE__bool_true;
  } else {
    /*___UNKNOWN_METHOD___;*/
  }
  SMOP_RELEASE(interpreter,invocant);
  SMOP_RELEASE(interpreter,capture);
  return ret;
}
static void DESTROYALL(SMOP__Object* interpreter,SMOP__Object* value) {
     smop_nagc_rdlock((SMOP__NAGC__Object*)value);
     void (*destr)(SMOP__Object* interpreter,
                 SMOP__Object* frame);
     destr = ((SMOP__LOST__Frame*)value)->destr;

     if (((SMOP__LOST__Frame*)value)->back) ((SMOP__LOST__Frame*)value)->back = NULL;

     smop_nagc_unlock((SMOP__NAGC__Object*)value);
     destr(interpreter,value);
}

SMOP__Object* SMOP__LOST__Frame_create(SMOP__Object* interpreter,
                                       SMOP__Object* back,
                                       void* user,
                                       int (*step)(SMOP__Object* interpreter,
                                                   SMOP__Object* frame),
                                       void (*destr)(SMOP__Object* interpreter,
                                                    SMOP__Object* frame)) {
  SMOP__LOST__Frame* frame = (SMOP__LOST__Frame*) smop_nagc_alloc(sizeof(SMOP__LOST__Frame));
  frame->RI = (SMOP__ResponderInterface*) RI;
  frame->back = back;
  frame->user = user;
  frame->pc = 0;
  frame->lastr = SMOP__NATIVE__bool_false;
  frame->step = step;
  frame->destr = destr;
  return (SMOP__Object*) frame;
}

void smop_lost_init() {
  SMOP__ID__eval = SMOP__NATIVE__idconst_create("eval");
  SMOP__ID__goto = SMOP__NATIVE__idconst_create("goto");
  RI = (SMOP__Object*) SMOP__NAGC__RI__create(MESSAGE,smop_nagc_reference,smop_nagc_release,smop_nagc_weakref,DESTROYALL,"lost frame");
}

void smop_lost_destr() {
  free(RI);
}
