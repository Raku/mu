#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/s1p.h>
#include <stdio.h>
SMOP__Object* SMOP__S1P__LexicalPrelude;
void smop_s1p_init(SMOP__Object* interpreter) {
  smop_s1p_proto_init(interpreter);
  smop_s1p_scalar_init(interpreter);
  smop_s1p_hash_init(interpreter);
  smop_s1p_hash_bvalue_init(interpreter);
  smop_s1p_lexicalscope_init(interpreter);
  smop_s1p_code_init(interpreter);
  smop_s1p_io_init(interpreter);
  smop_s1p_lexical_prelude_init(interpreter);
}
void smop_s1p_destr(SMOP__Object* interpreter) {
  smop_s1p_lexical_prelude_destr(interpreter);
  smop_s1p_io_destr(interpreter);
  smop_s1p_code_destr(interpreter);
  smop_s1p_lexicalscope_destr(interpreter);
  smop_s1p_hash_bvalue_destr(interpreter);
  smop_s1p_hash_destr(interpreter);
  smop_s1p_scalar_destr(interpreter);
  smop_s1p_proto_destr(interpreter);
}
