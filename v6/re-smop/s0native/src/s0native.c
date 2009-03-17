#include <smop_base.h>
#include <smop_s0native.h>
#include <stdlib.h>

void smop_s0native_init(void) {
  smop_native_bool_init();
  smop_idconst_init();
}

void smop_s0native_destr(void) {
  smop_native_bool_destr();
  smop_idconst_destr();
}

SMOP__Object* smop_placeholder_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  fprintf(stderr,"%s MESSAGE is not filled in yet\n",abort);
  abort();
}

SMOP__Object* smop_noop_reference(SMOP__Object* interpreter,
                                    SMOP__ResponderInterface* responder,
                                    SMOP__Object* obj) {
  return obj;
}

SMOP__Object* smop_noop_release(SMOP__Object* interpreter,
                                  SMOP__ResponderInterface* responder,
                                  SMOP__Object* obj) {
  return obj;
}

SMOP__Object* smop_noop_weakref(SMOP__Object* interpreter,
                                SMOP__ResponderInterface* responder,
                                SMOP__Object* obj) {
  return obj;
}
