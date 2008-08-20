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
  SMOP__Object* mold;
  SMOP__Object* signature;
  SMOP__Object* outer ;
} SMOP__S1P__Code_struct;

SMOP__Object* SMOP__S1P__Code_create() {
    SMOP__Object* ret = smop_lowlevel_alloc(sizeof(SMOP__S1P__Code_struct));
    ret->RI = (SMOP__ResponderInterface*)SMOP__S1P__Code;
    return ret;
}

static SMOP__Object* lowlevel_code_message(SMOP__Object* interpreter,
                                           SMOP__ResponderInterface* self,
                                           SMOP__Object* identifier,
                                           SMOP__Object* capture) {
  ___NATIVE_CAPTURE_ONLY___;
  ___CONST_IDENTIFIER_ONLY___;
  ___INVOCANT_RI_SHOULD_MATCH___;

  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  if (SMOP__ID__new == identifier) {
    ret = SMOP__S1P__Code_create(SMOP__NATIVE__bool_false);
    SMOP__S1P__Code_struct* code = (SMOP__S1P__Code_struct*) ret;
    code->mold = SMOP__NATIVE__capture_named(interpreter,SMOP_REFERENCE(interpreter,capture),SMOP__ID__mold);
    code->outer = SMOP__NATIVE__capture_named(interpreter,SMOP_REFERENCE(interpreter,capture),SMOP__ID__outer);
    code->signature = SMOP__NATIVE__capture_named(interpreter,SMOP_REFERENCE(interpreter,capture),SMOP__ID__signature);

  } else if (SMOP__ID__postcircumfix_parens == identifier) {
    SMOP__S1P__Code_struct* code = (SMOP__S1P__Code_struct*) invocant;

    smop_lowlevel_rdlock(code);
    SMOP__Object* mold = code->mold;
    smop_lowlevel_unlock(code);

    SMOP__Object* frame = SMOP__Mold__Frame_create(interpreter,SMOP_REFERENCE(interpreter,mold));

    SMOP_DISPATCH(interpreter,SMOP_RI(interpreter),SMOP__ID__goto,frame);


  } else if (SMOP__ID__DESTROYALL == identifier) {

    SMOP__S1P__Code_struct* code = (SMOP__S1P__Code_struct*) invocant;

    smop_lowlevel_rdlock(code);
    SMOP__Object* mold = code->mold;
    smop_lowlevel_unlock(code);

    //SMOP_RELEASE(interpreter,mold);

  } else {
    ___UNKNOWN_METHOD___;
  }
  SMOP_RELEASE(interpreter,invocant);
  SMOP_RELEASE(interpreter,capture);
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
