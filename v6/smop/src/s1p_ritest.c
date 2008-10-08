#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_identifiers.h>
#include <smop_s1p.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

SMOP__Object* SMOP__S1P__ritest;

static SMOP__Object* ccode (SMOP__Object* interpreter,
                            SMOP__Object* ccode,
                            SMOP__Object* capture) {
  ___NATIVE_CAPTURE_ONLY___;
  SMOP__Object* ret;
  SMOP__Object* val = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
  SMOP__Object* ri = SMOP__NATIVE__capture_positional(interpreter, capture, 1);
  if (SMOP_RI(val) == (SMOP__ResponderInterface*)ri) {
    ret = SMOP__NATIVE__bool_true;
  } else {
    ret = SMOP__NATIVE__bool_false;
  }
  SMOP_RELEASE(interpreter, val);
  SMOP_RELEASE(interpreter, ri);
  SMOP_RELEASE(interpreter, capture);
  return ret;
}

void smop_s1p_ritest_init() {
  SMOP__S1P__ritest = SMOP__S1P__CCode_create(ccode);
}

void smop_s1p_ritest_destr() {
  SMOP_RELEASE(SMOP__GlobalInterpreter, SMOP__S1P__ritest);
  SMOP__S1P__ritest = NULL;
}
