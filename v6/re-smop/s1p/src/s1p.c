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
  smop_s1p_loader_init(interpreter);
  smop_s1p_adhocsignature_init(interpreter);
  smop_s1p_package_init(interpreter);
  smop_s1p_ccode_init(interpreter);
  smop_s1p_flattenedscope_init(interpreter);
  smop_s1p_ritest_init(interpreter);
  smop_s1p_capturize_init(interpreter);
  control_exception_return_init(interpreter);
  smop_s1p_lexical_prelude_init(interpreter);
}
void smop_s1p_destr(SMOP__Object* interpreter) {
  smop_s1p_lexical_prelude_destr(interpreter);
  control_exception_return_destr(interpreter);
  smop_s1p_capturize_destr(interpreter);
  smop_s1p_ritest_destr(interpreter);
  smop_s1p_flattenedscope_destr(interpreter);
  smop_s1p_ccode_destr(interpreter);
  smop_s1p_package_destr(interpreter);
  smop_s1p_adhocsignature_destr(interpreter);
  smop_s1p_loader_destr(interpreter);
  smop_s1p_io_destr(interpreter);
  smop_s1p_code_destr(interpreter);
  smop_s1p_lexicalscope_destr(interpreter);
  smop_s1p_hash_bvalue_destr(interpreter);
  smop_s1p_hash_destr(interpreter);
  smop_s1p_scalar_destr(interpreter);
  smop_s1p_proto_destr(interpreter);
}
