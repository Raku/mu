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
  smop_lowlevel_wrlock(object);
  SMOP__Object* old = ((SMOP__S1P__Scalar_struct*)object)->cell;
  ((SMOP__S1P__Scalar_struct*)object)->cell = val;
  smop_lowlevel_unlock(object);
  return old;
}

static SMOP__Object* smop_s1p_scalar_message(SMOP__Object* interpreter,
                                             SMOP__ResponderInterface* responder,
                                             SMOP__Object* identifier,
                                             SMOP__Object* capture) {
  ___NATIVE_CAPTURE_ONLY___;
  ___CONST_IDENTIFIER_ONLY___;
  SMOP__Object* scalar = SMOP__NATIVE__capture_invocant(interpreter, capture);
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  if (SMOP__ID__FETCH == identifier) {

    ret = SMOP__S1P__Scalar_FETCH(scalar);
    if (ret)
      SMOP_REFERENCE(interpreter,ret);

  } else if (SMOP__ID__STORE == identifier) {

    SMOP__Object* value = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
    SMOP__Object* old = SMOP__S1P__Scalar_STORE(scalar,value);
    if (old) SMOP_RELEASE(interpreter,old);

  } else if (SMOP__ID__DESTROYALL == identifier) {

    SMOP__S1P__Scalar_struct* s = (SMOP__S1P__Scalar_struct*)scalar;
    smop_lowlevel_wrlock(scalar);
    SMOP__Object* cell = s->cell; s->cell = NULL;
    smop_lowlevel_unlock(scalar);
    if (cell) SMOP_RELEASE(interpreter,cell);
    
  } else if (SMOP__ID__new == identifier) {

    ret = SMOP__S1P__Scalar_create(SMOP__NATIVE__bool_false);

  } else {
    ___UNKNOWN_METHOD___;

  }
  if (scalar) SMOP_RELEASE(interpreter,scalar);
  SMOP_RELEASE(interpreter,capture);
  return ret;
}
void smop_s1p_scalar_init() {
  SMOP__S1P__Scalar = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__Scalar)->MESSAGE = smop_s1p_scalar_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Scalar)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Scalar)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Scalar)->id = "S1P Scalar";
}

void smop_s1p_scalar_destr() {
  free(SMOP__S1P__Scalar);
}
