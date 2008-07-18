#include <stdlib.h>
#include <stdio.h>
#include <smop.h>
#include <string.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>

SMOP__Object* SMOP__S1P__RootNamespace;

void smop_s1p_root_namespace_init() {
  SMOP__S1P__RootNamespace = SMOP__S1P__Hash_create();
}
void smop_s1p_root_namespace_insert(SMOP__Object* interpreter,char* name,char* obj) {

  SMOP__Object* cell = SMOP_DISPATCH(interpreter,
                                     SMOP_RI(SMOP__S1P__RootNamespace),
                                     SMOP__ID__postcircumfix_curly,
                                     SMOP__NATIVE__capture_create(interpreter,
                                                                  SMOP__S1P__RootNamespace,
                                                                  (SMOP__Object*[]) {SMOP__S1P__Str_create(name),NULL},
                                                                  NULL));

  SMOP_RELEASE(interpreter,
               SMOP_DISPATCH(cell,
                             SMOP_RI(cell),
                             SMOP__ID__STORE,
                             SMOP__NATIVE__capture_create(interpreter,
                                                          SMOP__S1P__RootNamespace,
                                                          (SMOP__Object*[]) {obj,NULL},
                                                          NULL)));
}

void smop_s1p_root_namespace_destr() {
  SMOP_RELEASE(SMOP__GlobalInterpreter,SMOP__S1P__RootNamespace);
}


