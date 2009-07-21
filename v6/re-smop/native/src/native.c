#include <smop/native.h>
void smop_native_init(SMOP__Object* interpreter) {
  smop_native_int_init(interpreter);

  smop_bool_message_init(interpreter);
  smop_capture_message_init(interpreter);
  smop_idconst_message_init(interpreter);
}
 
void smop_native_destr(SMOP__Object* interpreter) {
  smop_idconst_message_destr(interpreter);
  smop_capture_message_destr(interpreter);
  smop_bool_message_destr(interpreter);

  smop_native_int_destr(interpreter);
}
