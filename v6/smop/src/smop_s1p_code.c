#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_identifiers.h>
#include <smop_s1p.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

SMOP__Object* SMOP__S1P__Code;

/*
 * SMOP__S1P__Code passes all this items as the ownership of the
 * objects. Which means that the method is responsible for releasing
 * the object.
 *
 * This is important to allow the method to pass the object to another
 * call without having to do an additional call to REFERENCE.
 */
typedef struct SMOP__S1P__Code_struct {
  SMOP__Object__BASE
  SMOP__Object* (*code) (SMOP__Object* interpreter,
                         SMOP__Object* code,
                         SMOP__Object* capture);
} SMOP__S1P__Code_struct;

static SMOP__Object* lowlevel_code_message(SMOP__Object* interpreter,
                                           SMOP__ResponderInterface* self,
                                           SMOP__Object* identifier,
                                           SMOP__Object* capture) {
  ___NATIVE_CAPTURE_ONLY___;
  ___CONST_IDENTIFIER_ONLY___;

  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  if (SMOP__ID__call == identifier) {
    ___INVOCANT_RI_SHOULD_MATCH___;

    SMOP__S1P__Code_struct* c = (SMOP__S1P__Code_struct*)invocant;

    SMOP__Object* actualcap = SMOP__NATIVE__capture_positional(interpreter,capture,0);

    smop_lowlevel_rdlock(invocant);
    SMOP__Object* (*func) (SMOP__Object* interpreter,
                           SMOP__Object* code,
                           SMOP__Object* capture) = c->code;
    smop_lowlevel_unlock(invocant);

    ret = func(interpreter,invocant,actualcap);

    SMOP_RELEASE(interpreter,capture);
    SMOP_RELEASE(interpreter,invocant);

  } else if (SMOP__ID__DESTROYALL == identifier) {

    SMOP_RELEASE(interpreter,capture);

  } else {
    ___UNKNOWN_METHOD___;
  }
  return ret;
}


SMOP__Object* SMOP__S1P__Code_create(SMOP__Object* (*code) (SMOP__Object* interpreter,
                                                            SMOP__Object* code,
                                                            SMOP__Object* capture)) {

  SMOP__Object* ret = smop_lowlevel_alloc(sizeof(SMOP__S1P__Code_struct));
  ret->RI = (SMOP__ResponderInterface*)SMOP__S1P__Code;

  SMOP__S1P__Code_struct* m = (SMOP__S1P__Code_struct*)ret;
  m->code = code;

  return ret;

}


void smop_s1p_code_init() {
  SMOP__S1P__Code = malloc(sizeof(SMOP__ResponderInterface));
  assert(SMOP__S1P__Code);
  ((SMOP__ResponderInterface*)SMOP__S1P__Code)->RI = NULL;
  ((SMOP__ResponderInterface*)SMOP__S1P__Code)->MESSAGE = lowlevel_code_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Code)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Code)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Code)->id = "SMOP S1P Code";
}

void smop_s1p_code_destr() {
  free(SMOP__S1P__Code);
}
