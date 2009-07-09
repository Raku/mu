#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/yeast.h>
#include <smop/capture.h>
#include <smop/util.h>
#include <smop/mold.h>
#include <stdlib.h>

static SMOP__NAGC__ResponderInterface* RI;

static SMOP__Object* SMOP__ID__set_regs;

static SMOP__Object* SMOP__ID__set_back;
static SMOP__Object* SMOP__ID__back;
static SMOP__Object* SMOP__ID__set_control;
static SMOP__Object* SMOP__ID__control;
static SMOP__Object* SMOP__ID__set_catch;
static SMOP__Object* SMOP__ID__catch;
static SMOP__Object* SMOP__ID__set_lexical;
static SMOP__Object* SMOP__ID__lexical;

static SMOP__Object* SMOP__ID__setr;

static SMOP__Object* SMOP__ID__new;
static SMOP__Object* SMOP__ID__eval;

static SMOP__Object* SMOP__ID__STORE;
static SMOP__Object* SMOP__ID__FETCH;
static SMOP__Object* SMOP__ID__true;

static SMOP__Object* MESSAGE(SMOP__Object* interpreter,
                            SMOP__ResponderInterface* self,
                            SMOP__Object* identifier,
                            SMOP__Object* capture) {
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  SMOP__Object* invocant = SMOP__NATIVE__capture_positional(interpreter,capture,0);

  SMOP__Yeast__Frame* frame = (SMOP__Yeast__Frame*) invocant;

  if (SMOP__ID__eval == identifier) {
    smop_nagc_rdlock((SMOP__NAGC__Object*)invocant);


    void (*step)(SMOP__Object* interpreter,
                SMOP__Object* frame);
    step = ((SMOP__Yeast__Frame*)invocant)->step;
    smop_nagc_unlock((SMOP__NAGC__Object*)invocant);
    step(interpreter,invocant);
    if (((SMOP__Yeast__Frame*)invocant)->pc == -1) {
      ret = SMOP__NATIVE__bool_false;
    } else {
      ret = SMOP__NATIVE__bool_true;
    }

  } else if (SMOP__ID__setr == identifier) {
    SMOP__Object* value = SMOP__NATIVE__capture_positional(interpreter, capture, 1);

    if (value == invocant) {
      SMOP_RELEASE(interpreter, value);
    }
    SMOP__Object** target = ((SMOP__Yeast__Frame*)invocant)->ret;
    if (!target) fprintf(stderr,"calling setr on a frame not expecting a return value\n");
    if (*target) {
      SMOP_RELEASE(interpreter,ret);
    }
    *target = value;

  } else if (SMOP__ID__set_regs == identifier) {
    int i;
    int count = SMOP__NATIVE__capture_positional_count(interpreter, capture);
    for (i = 1;i < count;i++) {
      SMOP__Object* value = SMOP__NATIVE__capture_positional(interpreter, capture, i);
      smop_reg_set(interpreter, invocant, i-1, value);
    }

  } else if (SMOP__ID__back == identifier) {
    if (frame->back) {
      ret = SMOP_REFERENCE(interpreter,frame->back);
    }
  } else if (SMOP__ID__control == identifier) {
    if (frame->control) {
      ret = SMOP_REFERENCE(interpreter,frame->control);
    }
  } else if (SMOP__ID__catch == identifier) {
    if (frame->catch) {
      ret = SMOP_REFERENCE(interpreter,frame->catch);
    }
  } else if (SMOP__ID__lexical == identifier) {
    if (frame->lexical) {
      ret = SMOP_REFERENCE(interpreter,frame->lexical);
    }

  } else if (SMOP__ID__set_back == identifier) {
    SMOP__Object* value = SMOP__NATIVE__capture_positional(interpreter, capture, 1);
    if (!frame->back) {
      frame->back = value;
    } else {
      printf("trying to set a new back to the frame\n");
      abort();
    }

  } else if (SMOP__ID__set_control == identifier) {
    SMOP__Object* value = SMOP__NATIVE__capture_positional(interpreter, capture, 1);
    if (!frame->control) {
      frame->control = value;
    } else {
      printf("trying to set a new control to the frame\n");
      abort();
    }

  } else if (SMOP__ID__set_catch == identifier) {
    SMOP__Object* value = SMOP__NATIVE__capture_positional(interpreter, capture, 1);
    if (!frame->catch) {
      frame->catch = value;
    } else {
      printf("trying to set a new lexical to the frame\n");
      abort();
    }

  } else if (SMOP__ID__set_lexical == identifier) {
    SMOP__Object* value = SMOP__NATIVE__capture_positional(interpreter, capture, 1);
    if (!frame->lexical) {
      frame->lexical = value;
    } else {
      printf("trying to set a new lexical to the frame\n");
      abort();
    }
  } else if (identifier == SMOP__ID__FETCH) {
    ___VALUE_FETCH___;

  } else if (identifier == SMOP__ID__STORE) {
    ___VALUE_STORE___;

  } else if (SMOP__ID__true == identifier) {
    ret = SMOP__NATIVE__bool_true;

  } else {
    ___UNKNOWN_METHOD___;
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
    ret->yeast = yeast_object;

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

#ifdef SMOP_PROFILE
      if (SMOP_PROFILE_on) fprintf(SMOP_PROFILE_out,"creating frame %p\n",ret);
#endif
    return (SMOP__Object*) ret;
}
SMOP__Object* SMOP__Frame_create(SMOP__Object* interpreter,SMOP__Object* shape) {
  if (shape->RI == (SMOP__ResponderInterface*)SMOP__Yeast__RI) {
    return SMOP__Yeast__Frame_create(interpreter,shape);
  } else {
    return SMOP__Mold__Frame_create(interpreter,shape);
  }
}

void smop_reg_set(SMOP__Object* interpreter,SMOP__Object* frame, int regnum, SMOP__Object* value) {
  if (frame->RI == (SMOP__ResponderInterface*)RI)
    yeast_reg_set(interpreter,frame,regnum,value);
  else
    mold_reg_set(interpreter,frame,regnum,value);
}

void smop_back_set(SMOP__Object* interpreter,SMOP__Object* obj, SMOP__Object* value) {
    if (obj->RI == (SMOP__ResponderInterface*)RI) {
      SMOP__Yeast__Frame* frame = (SMOP__Yeast__Frame*) obj;
      if (frame->back) {
        printf("setting back , previous %s new = %s\n",frame->back->RI->id,value->RI->id);
        printf("Cannot redefine mold back!");
        abort();
      }
      frame->back = value;
    } else {
      mold_back_set(interpreter,obj,value);
    }
}

void yeast_reg_set(SMOP__Object* interpreter,SMOP__Object* yeastframe, int regnum, SMOP__Object* value) {
    SMOP__Yeast__Frame* frame = (SMOP__Yeast__Frame*) yeastframe;
    SMOP__Yeast* yeast = (SMOP__Yeast*) frame->yeast;
    int where = yeast->constants_len+regnum;
    SMOP__Object* old = frame->reg[where];
//    printf("setting %d to %s\n",where,value->RI->id);
    frame->reg[where] = value;
    if (old) {
      SMOP_RELEASE(interpreter, old);
    }
}

void smop_yeast_frame_init() {
  SMOP__ID__set_regs = SMOP__NATIVE__idconst_create("set_regs");

  SMOP__ID__set_back = SMOP__NATIVE__idconst_create("set_back");
  SMOP__ID__back = SMOP__NATIVE__idconst_create("back");
  SMOP__ID__set_control = SMOP__NATIVE__idconst_create("set_control");
  SMOP__ID__control = SMOP__NATIVE__idconst_create("control");
  SMOP__ID__set_catch = SMOP__NATIVE__idconst_create("set_catch");
  SMOP__ID__catch = SMOP__NATIVE__idconst_create("catch");
  SMOP__ID__set_lexical = SMOP__NATIVE__idconst_create("set_lexical");
  SMOP__ID__lexical = SMOP__NATIVE__idconst_create("lexical");

  SMOP__ID__setr = SMOP__NATIVE__idconst_create("setr");

  SMOP__ID__new = SMOP__NATIVE__idconst_create("new");
  SMOP__ID__eval = SMOP__NATIVE__idconst_create("eval");

  SMOP__ID__STORE = SMOP__NATIVE__idconst_create("STORE");
  SMOP__ID__FETCH = SMOP__NATIVE__idconst_create("FETCH");
  SMOP__ID__true = SMOP__NATIVE__idconst_create("true");

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
