#include <stdio.h>
#include <stdlib.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>

static SMOP__Object* RI;
SMOP__Object* SMOP__S1P__Scalar;

typedef struct SMOP__S1P__Scalar_struct {
  SMOP__Object__BASE
  SMOP__Object* cell;
} SMOP__S1P__Scalar_struct;


SMOP__Object* SMOP__S1P__Scalar_create(SMOP__Object* initial_value) {
  assert(initial_value);
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

  } else if (SMOP__ID__bool == identifier) {
    SMOP__Object* cell = SMOP__S1P__Scalar_FETCH(scalar);
    ret = SMOP_DISPATCH(interpreter, SMOP_RI(cell),
                        SMOP__ID__bool,
                        SMOP__NATIVE__capture_delegate(interpreter,
                                                       SMOP_REFERENCE(interpreter,cell),
                                                       SMOP_REFERENCE(interpreter,capture)));


  } else if (SMOP__ID__STORE == identifier) {

    SMOP__Object* value = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
    if (!value) printf("storing null pointer ugh\n");
    smop_lowlevel_wrlock(scalar);
    SMOP__Object* old = ((SMOP__S1P__Scalar_struct*)scalar)->cell;
    ((SMOP__S1P__Scalar_struct*)scalar)->cell = value;
    smop_lowlevel_unlock(scalar);
    if (old) SMOP_RELEASE(interpreter,old);
    ret = value;
    SMOP_REFERENCE(interpreter,ret);

  } else if (SMOP__ID__DESTROYALL == identifier) {

    SMOP__S1P__Scalar_struct* s = (SMOP__S1P__Scalar_struct*)scalar;
    smop_lowlevel_wrlock(scalar);
    SMOP__Object* cell = s->cell; s->cell = NULL;
    smop_lowlevel_unlock(scalar);
    if (cell) SMOP_RELEASE(interpreter,cell);
    
  } else if (SMOP__ID__new == identifier) {

    SMOP__Object* cell = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
    ret = SMOP__S1P__Scalar_create(cell);

  } else {
    ___UNKNOWN_METHOD___;

  }
  if (scalar) SMOP_RELEASE(interpreter,scalar);
  SMOP_RELEASE(interpreter,capture);
  return ret;
}
void smop_s1p_scalar_init() {
  RI = SMOP__RI__create(smop_s1p_scalar_message,
                        smop_lowlevel_generic_reference,
                        smop_lowlevel_generic_release,
                        "S1P Scalar");
  SMOP__S1P__Scalar = SMOP__Proto__create(RI);
}

void smop_s1p_scalar_destr() {
  free(SMOP__S1P__Scalar);
  free(RI);
}
