#include <smop/base.h>
#include <smop/s1p-oo.h>
void smop_s1p_oo_init(SMOP__Object* interpreter) {
  smop_pureprototypehow_init(interpreter);
  smop_s1p_oo_attribute_init(interpreter);
}
void smop_s1p_oo_destr(SMOP__Object* interpreter) {
  smop_s1p_oo_attribute_destr(interpreter);
  smop_pureprototypehow_destr(interpreter);
}
