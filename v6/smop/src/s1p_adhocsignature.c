#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>
#include <smop_mold.h>

SMOP__Object* SMOP__S1P__AdhocSignature;

typedef struct smop_s1p_adhocsignature_struct {
  SMOP__Object__BASE
  SMOP__Object* accepts_mold;
  SMOP__Object* bind_mold;
} smop_s1p_adhocsignature_struct;


static SMOP__Object* adhocsignature_message(SMOP__Object* interpreter,
                                                   SMOP__ResponderInterface* self,
                                                   SMOP__Object* identifier,
                                                   SMOP__Object* capture) {

  ___NATIVE_CAPTURE_ONLY___;
  ___CONST_IDENTIFIER_ONLY___;
  ___INVOCANT_RI_SHOULD_MATCH___;

  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  
  if (SMOP__ID__new == identifier) {
    ret = smop_lowlevel_alloc(sizeof(smop_s1p_adhocsignature_struct));
    smop_s1p_adhocsignature_struct* sig = (smop_s1p_adhocsignature_struct*)ret;
    sig->RI = (SMOP__ResponderInterface*)SMOP__S1P__AdhocSignature;
    sig->accepts_mold = SMOP__NATIVE__capture_named(interpreter,capture,SMOP__ID__ACCEPTS);
    sig->bind_mold = SMOP__NATIVE__capture_named(interpreter,capture,SMOP__ID__BIND);

  } else if (identifier == SMOP__ID__BIND) {

    smop_lowlevel_rdlock(invocant);
    SMOP__Object* mold = ((smop_s1p_adhocsignature_struct*)invocant)->bind_mold;
    smop_lowlevel_unlock(invocant);

    SMOP__Object* continuation = SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                                               SMOP__ID__continuation,
                                               SMOP__NATIVE__capture_create(interpreter,
                                                                            SMOP_REFERENCE(interpreter,interpreter),
                                                                            NULL,NULL));

    SMOP__Object* frame = SMOP__Mold__Frame_create(interpreter,
                                                   SMOP_REFERENCE(interpreter,mold));

    mold_reg_set(interpreter,frame,0,SMOP_REFERENCE(interpreter,capture));
    mold_reg_set(interpreter,frame,1,continuation);
    mold_reg_set(interpreter,frame,2,SMOP_REFERENCE(interpreter,interpreter));

    SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                  SMOP__ID__goto,
                  frame);

  } else if (identifier == SMOP__ID__ACCEPTS) {

    smop_lowlevel_rdlock(invocant);
    SMOP__Object* mold = ((smop_s1p_adhocsignature_struct*)invocant)->accepts_mold;
    smop_lowlevel_unlock(invocant);

    SMOP__Object* continuation = SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                                               SMOP__ID__continuation,
                                               SMOP__NATIVE__capture_create(interpreter,
                                                                            SMOP_REFERENCE(interpreter,interpreter),
                                                                            NULL,NULL));

    SMOP__Object* frame = SMOP__Mold__Frame_create(interpreter,
                                                   SMOP_REFERENCE(interpreter,mold));

    mold_reg_set(interpreter,frame,0,SMOP_REFERENCE(interpreter,capture));
    mold_reg_set(interpreter,frame,1,continuation);
    mold_reg_set(interpreter,frame,2,SMOP_REFERENCE(interpreter,interpreter));

    SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                  SMOP__ID__goto,
                  frame);


  } else if (identifier == SMOP__ID__DESTROYALL) {

    smop_lowlevel_wrlock(invocant);
    SMOP__Object* bmold = ((smop_s1p_adhocsignature_struct*)invocant)->bind_mold;
    ((smop_s1p_adhocsignature_struct*)invocant)->bind_mold = NULL;
    SMOP__Object* amold = ((smop_s1p_adhocsignature_struct*)invocant)->accepts_mold;
    ((smop_s1p_adhocsignature_struct*)invocant)->accepts_mold = NULL;
    smop_lowlevel_unlock(invocant);

    if (bmold) SMOP_RELEASE(interpreter,bmold);
    if (amold) SMOP_RELEASE(interpreter,amold);
    
  } else {
    ___UNKNOWN_METHOD___;

  }

  SMOP_RELEASE(interpreter, invocant);
  SMOP_RELEASE(interpreter, capture);

  return ret;
}

void smop_s1p_adhocsignature_init() {
  SMOP__S1P__AdhocSignature = calloc(1, sizeof(SMOP__ResponderInterface));
  assert(SMOP__S1P__AdhocSignature);
  ((SMOP__ResponderInterface*)SMOP__S1P__AdhocSignature)->MESSAGE = adhocsignature_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__AdhocSignature)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__AdhocSignature)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__AdhocSignature)->id = "S1P Adhoc Signature";
}

void smop_s1p_adhocsignature_destr() {
  free(SMOP__S1P__AdhocSignature);
}
