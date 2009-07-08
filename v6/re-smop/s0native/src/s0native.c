#include <smop/base.h>
#include <smop/s0native.h>
#include <stdlib.h>
#include <stdio.h>

void smop_s0native_init(void) {
  smop_metaRI_init();
  smop_native_bool_init();
  smop_idconst_init();
  smop_empty_interpreter_init();
#ifdef SMOP_PROFILE
  smop_profile_init();
#endif
}

void smop_s0native_destr(void) {
  smop_empty_interpreter_destr();
  smop_idconst_destr();
  smop_native_bool_destr();
  smop_metaRI_destr();
}

SMOP__Object* smop_placeholder_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  int retsize;
  fprintf(stderr,"MESSAGE is not filled in yet: %s RI: %s\n",SMOP__NATIVE__idconst_fetch_with_null(identifier,&retsize),self->id);
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
