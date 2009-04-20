#include <smop/base.h>
#include <smop/s1p.h>
#include <smop/capture.h>
#include <stdlib.h>
#include <smop/s0native.h>
#include <stdio.h>
static SMOP__Object* SMOP__ID__bind_key;
static SMOP__Object* SMOP__ID__entries;
void smop_s1p_lexical_prelude_insert(SMOP__Object* interpreter,char* name,SMOP__Object* obj) {
  SMOP__Object* entries = SMOP_DISPATCH(interpreter,
    SMOP_RI(SMOP__S1P__LexicalPrelude),
    SMOP__ID__entries,
    SMOP__NATIVE__capture_create(interpreter,
      (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,SMOP__S1P__LexicalPrelude),NULL},
      (SMOP__Object*[]) {NULL}
    )
  );

  SMOP_DISPATCH(interpreter,
    SMOP_RI(entries),
    SMOP__ID__bind_key,
    SMOP__NATIVE__capture_create(interpreter,
      (SMOP__Object*[]) {entries,SMOP__NATIVE__idconst_create(name),obj,NULL},
      (SMOP__Object*[]) {NULL}
    )
  );
}
void smop_s1p_lexical_prelude_init(SMOP__Object* interpreter) {
  SMOP__S1P__LexicalPrelude = SMOP_DISPATCH(
    interpreter,
    SMOP_RI(SMOP__S1P__LexicalScope),
    SMOP__NATIVE__idconst_create("new"),
    SMOP__NATIVE__capture_create(interpreter,(SMOP__Object*[]) {SMOP_REFERENCE(interpreter,SMOP__S1P__LexicalScope),NULL},(SMOP__Object*[]) {NULL})
  );
  SMOP__ID__entries = SMOP__NATIVE__idconst_create("entries");
  SMOP__ID__bind_key = SMOP__NATIVE__idconst_create("bind_key");
  smop_s1p_lexical_prelude_insert(interpreter,"Code",SMOP_REFERENCE(interpreter,SMOP__S1P__Code));
  smop_s1p_lexical_prelude_insert(interpreter,"capture",SMOP__Proto__create(SMOP__capture__RI));
  smop_s1p_lexical_prelude_insert(interpreter,"$OUT",SMOP__S1P__IO_create(interpreter));
  smop_s1p_lexical_prelude_insert(interpreter,"MildewSOLoader",SMOP_REFERENCE(interpreter,SMOP__MildewSOLoader));
  smop_s1p_lexical_prelude_insert(interpreter,"$LexicalPrelude",SMOP_REFERENCE(interpreter,SMOP__S1P__LexicalPrelude));
  smop_s1p_lexical_prelude_insert(interpreter,"AdhocSignature",SMOP_REFERENCE(interpreter,SMOP__S1P__AdhocSignature));
  smop_s1p_lexical_prelude_insert(interpreter,"Scalar",SMOP_REFERENCE(interpreter,SMOP__S1P__Scalar));
}

void smop_s1p_lexical_prelude_destr(SMOP__Object* interpreter) {
  SMOP_RELEASE(interpreter,SMOP__S1P__LexicalPrelude);
}
