#include <smop/base.h>
#include <smop/native.h>
#include <smop/capture.h>
#include <smop/s0native.h>
#include <smop/util.h>
static SMOP__Object* SMOP__ID__positional;
static SMOP__Object* message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  SMOP__Object* invocant = SMOP__NATIVE__capture_positional(interpreter,capture,0);
  if (identifier == SMOP__ID__positional) {
    SMOP__Object* i = SMOP__NATIVE__capture_positional(interpreter,capture,1);
    ret = SMOP__NATIVE__capture_positional(interpreter,invocant,SMOP__NATIVE__int_fetch(i));
    SMOP_RELEASE(interpreter,i);
  } else {
    ___UNKNOWN_METHOD___;
  }
  SMOP_RELEASE(interpreter, invocant);
  SMOP_RELEASE(interpreter, capture);
  return ret;
}
void smop_capture_message_init(SMOP__Object* interpreter) {
  ((SMOP__ResponderInterface*)SMOP__capture__RI)->MESSAGE = message;
  SMOP__ID__positional = SMOP__NATIVE__idconst_create("positional");

}
void smop_capture_message_destr(SMOP__Object* interpreter) {
}
