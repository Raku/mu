#include <smop/base.h>
#include <smop/native.h>
#include <smop/capture.h>
#include <smop/s0native.h>
#include <smop/util.h>
static SMOP__Object* SMOP__ID__positional;
static SMOP__Object* SMOP__ID__new;
static SMOP__Object* SMOP__ID__named;
static SMOP__Object* SMOP__ID__elems;
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

  } else if (identifier == SMOP__ID__named) {
    SMOP__Object* key = SMOP__NATIVE__capture_positional(interpreter,capture,1);
    ret = SMOP__NATIVE__capture_named(interpreter,invocant,key);
    if (!ret) ret = SMOP__NATIVE__bool_false;
    SMOP_REFERENCE(interpreter,ret);

  } else if (identifier == SMOP__ID__elems) {
    int count = SMOP__NATIVE__capture_positional_count(interpreter,capture);
    ret = SMOP__NATIVE__int_create(count);

  } else if (identifier == SMOP__ID__new) {
    int count = SMOP__NATIVE__capture_positional_count(interpreter,capture);
    SMOP__Object** pos = malloc(sizeof(SMOP__Object*) * count);
    int i;
    for (i=1;i<count;i++) pos[i-1] = SMOP_REFERENCE(interpreter,SMOP__NATIVE__capture_positional(interpreter,capture,i));
    pos[count-1] = NULL;
    /*TODO named arguments */
    ret = SMOP__NATIVE__capture_create(interpreter,pos,(SMOP__Object*[]) {NULL});
    free(pos);

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
  SMOP__ID__new = SMOP__NATIVE__idconst_create("new");
  SMOP__ID__named = SMOP__NATIVE__idconst_create("named");
  SMOP__ID__elems = SMOP__NATIVE__idconst_create("elems");

}
void smop_capture_message_destr(SMOP__Object* interpreter) {
}
