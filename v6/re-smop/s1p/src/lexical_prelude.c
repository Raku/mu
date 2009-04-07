#include <smop/base.h>
#include <smop/s1p.h>
#include <smop/capture.h>
#include <stdlib.h>
#include <smop/s0native.h>
#include <stdio.h>
void smop_s1p_lexical_prelude_init(SMOP__Object* interpreter) {
  SMOP__S1P__LexicalPrelude = SMOP_DISPATCH(
    interpreter,
    SMOP_RI(SMOP__S1P__LexicalScope),
    SMOP__NATIVE__idconst_create("new"),
    SMOP__NATIVE__capture_create(interpreter,(SMOP__Object*[]) {SMOP_REFERENCE(interpreter,SMOP__S1P__LexicalScope),NULL},(SMOP__Object*[]) {NULL})
  );
}

void smop_s1p_lexical_prelude_destr(SMOP__Object* interpreter) {
  SMOP_RELEASE(interpreter,SMOP__S1P__LexicalPrelude);
}
