#include <stdio.h>
#include <stdlib.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>
#include <smop_slime.h>

SMOP__Object* SMOP__S1P__Scalar;

typedef struct SMOP__S1P__Scalar_struct {
  SMOP__Object__BASE
  SMOP__Object* cell;
} SMOP__S1P__Scalar_struct;


SMOP__Object* SMOP__S1P__Scalar_create(SMOP__Object* initial_value) {
  SMOP__Object* ret = smop_lowlevel_alloc(sizeof(SMOP__S1P__Scalar_struct));
  ret->RI = (SMOP__ResponderInterface*)SMOP__S1P__Scalar;
  ((SMOP__S1P__Scalar_struct*)ret)->cell = initial_value;
  return ret;
}

SMOP__Object* SMOP__S1P__Scalar_FETCH(SMOP__Object* object) {
  smop_lowlevel_rdlock(object);
  SMOP__Object* val = ((SMOP__S1P__Scalar_struct*)object)->cell;
  smop_lowlevel_unlock(object);
  return val;
}

SMOP__Object* SMOP__S1P__Scalar_STORE(SMOP__Object* object, SMOP__Object* val) {
  smop_lowlevel_rdlock(object);
  SMOP__Object* old = ((SMOP__S1P__Scalar_struct*)object)->cell;
  ((SMOP__S1P__Scalar_struct*)object)->cell = val;
  smop_lowlevel_unlock(object);
  return old;
}

static SMOP__Object* smop_s1p_scalar_message(SMOP__Object* interpreter,
                                             SMOP__ResponderInterface* responder,
                                             SMOP__Object* identifier,
                                             SMOP__Object* capture) {
  if (SMOP__ID__FETCH == identifier) {
    SMOP__Object* scalar = SMOP__NATIVE__capture_invocant(interpreter, capture);
    SMOP__Object* ret = SMOP__S1P__Scalar_FETCH(scalar);
    SMOP_REFERENCE(interpreter,ret);
    SMOP_RELEASE(interpreter,scalar);
    SMOP_RELEASE(interpreter,capture);
    return ret;
  } else if (SMOP__ID__STORE == identifier) {
    SMOP__Object* scalar = SMOP__NATIVE__capture_invocant(interpreter, capture);
    SMOP__Object* value = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
    SMOP__Object* old = SMOP__S1P__Scalar_STORE(scalar,value);
    if (old) SMOP_RELEASE(interpreter,old);
    SMOP_RELEASE(interpreter,scalar);
    SMOP_RELEASE(interpreter,capture);
  } else if (SMOP__ID__DESTROYALL == identifier) {
    SMOP__S1P__Scalar_struct* s = (SMOP__S1P__Scalar_struct*)capture;
    
    smop_lowlevel_wrlock(capture);
    SMOP__Object* cell = s->cell; s->cell = NULL;
    smop_lowlevel_unlock(capture);

    SMOP_RELEASE(interpreter,cell);
  } else {
    fprintf(stderr,"Unknown identifier in lowlevel method object invocation.\n");
    SMOP_RELEASE(interpreter,capture);
  }
  return SMOP__NATIVE__bool_false;
}

static SMOP__Object* smop_s1p_scalar_reference(SMOP__Object* interpreter,
                                               SMOP__ResponderInterface* responder,
                                               SMOP__Object* value) {
  if (SMOP__S1P__Scalar != value) {
    return smop_lowlevel_refcnt_inc(interpreter,responder,value);
  } else {
    return value;
  }
}

static SMOP__Object* smop_s1p_scalar_release(SMOP__Object* interpreter,
                                             SMOP__ResponderInterface* responder,
                                             SMOP__Object* value) {
  if (SMOP__S1P__Scalar != value) {
    return smop_lowlevel_refcnt_dec(interpreter,responder,value);
  } else {
    return value;
  }
}

void smop_s1p_scalar_init() {
  SMOP__S1P__Scalar = malloc(sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__Scalar)->MESSAGE = smop_s1p_scalar_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Scalar)->REFERENCE = smop_s1p_scalar_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Scalar)->RELEASE = smop_s1p_scalar_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Scalar)->id = "S1P Scalar";
}

void smop_s1p_scalar_destr() {
  free(SMOP__S1P__Scalar);
}
