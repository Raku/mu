#include <stdlib.h>
#include <stdio.h>
#include <smop.h>
#include <string.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>
#include <smop_mold.h>

SMOP__Object* SMOP__S1P__RootNamespace;

void smop_s1p_root_namespace_insert(SMOP__Object* interpreter,char* name,SMOP__Object* obj) {
  SMOP__Object* cell = SMOP_DISPATCH(interpreter,
                                     SMOP_RI(SMOP__S1P__RootNamespace),
                                     SMOP__ID__bind_key,
                                     SMOP__NATIVE__capture_create(interpreter,
                                                                  SMOP_REFERENCE(interpreter,SMOP__S1P__RootNamespace),
                                                                  (SMOP__Object*[]) {SMOP__NATIVE__idconst_create(name),obj,NULL},
                                                                  NULL));
}
void smop_s1p_root_namespace_init() {
  SMOP__S1P__RootNamespace = SMOP__S1P__Hash_create();
  smop_s1p_root_namespace_insert(SMOP__GlobalInterpreter,"::Hash", SMOP__S1P__Hash);
  smop_s1p_root_namespace_insert(SMOP__GlobalInterpreter,"::Array", SMOP__S1P__Array);
  smop_s1p_root_namespace_insert(SMOP__GlobalInterpreter,"$*OUT", SMOP__S1P__IO_create(SMOP__GlobalInterpreter));
  smop_s1p_root_namespace_insert(SMOP__GlobalInterpreter,"::Mold", SMOP__Mold);
  smop_s1p_root_namespace_insert(SMOP__GlobalInterpreter,"::MoldFrame", SMOP__Mold__Frame);
  smop_s1p_root_namespace_insert(SMOP__GlobalInterpreter,"::Code", SMOP__S1P__Code);
  smop_s1p_root_namespace_insert(SMOP__GlobalInterpreter,"::Scalar", SMOP__S1P__Scalar);
  smop_s1p_root_namespace_insert(SMOP__GlobalInterpreter,"::MoldFrame", SMOP__Mold__Frame);
}

void smop_s1p_root_namespace_destr() {
  SMOP_RELEASE(SMOP__GlobalInterpreter,SMOP__S1P__RootNamespace);
}


