#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/yeast.h>
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


     void (*step)(SMOP__Object* interpreter,
                 SMOP__Object* frame);
     step = ((SMOP__Yeast__Frame*)invocant)->step;
     smop_nagc_unlock((SMOP__NAGC__Object*)invocant);
     step(interpreter,invocant);
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

     smop_nagc_unlock((SMOP__NAGC__Object*)value);
}

SMOP__Object* SMOP__Yeast__Frame_create(SMOP__Object* interpreter,SMOP__Object* yeast_object) {
    SMOP__Yeast* yeast = (SMOP__Yeast*) yeast_object;
    SMOP__Yeast__Frame* ret = (SMOP__Yeast__Frame*) smop_nagc_alloc(sizeof(SMOP__Yeast__Frame));
    ret->RI = (SMOP__ResponderInterface*)RI;
    ret->step = yeast->step;

    ret->pc = 0;
    ret->back = NULL;
    ret->reg = (SMOP__Object**) calloc(yeast->registers,sizeof(SMOP__ResponderInterface)); 
    ret->ret = NULL;

    int i;
    for (i = 0; i < yeast->constants_len; i++) {
      if (yeast->constants[i]) {
        ret->reg[i] = SMOP_REFERENCE(interpreter,yeast->constants[i]);
      }
    }

    return (SMOP__Object*) ret;
}

void smop_yeast_frame_init() {
  SMOP__ID__eval = SMOP__NATIVE__idconst_create("eval");
  SMOP__ID__goto = SMOP__NATIVE__idconst_create("goto");
  RI = calloc(1,sizeof(SMOP__NAGC__ResponderInterface));
  RI->RI = (SMOP__NAGC__ResponderInterface*)SMOP__metaRI;
  ((SMOP__NAGC__ResponderInterface*)RI)->MESSAGE = MESSAGE;
  ((SMOP__NAGC__ResponderInterface*)RI)->DESTROYALL = DESTROYALL;
  ((SMOP__NAGC__ResponderInterface*)RI)->REFERENCE = smop_nagc_reference;
  ((SMOP__NAGC__ResponderInterface*)RI)->RELEASE = smop_nagc_release;
  ((SMOP__NAGC__ResponderInterface*)RI)->id = "yeast frame";
}

void smop_yeast_frame_destr() {
  free(RI);
}
