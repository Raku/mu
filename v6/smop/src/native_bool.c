
#include <smop.h>

SMOP__Object* SMOP__NATIVE__bool;
static SMOP__Object* smop_native_bool_const_true;
static SMOP__Object* smop_native_bool_const_false;

static SMOP__Object* bool_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  //todo
  return self;
}

static SMOP__Object* capture_reference(SMOP__Object* interpreter,
                                       SMOP__ResponderInterface* responder,
                                       SMOP__Object* obj) {
  return obj;
}

static SMOP__Object* capture_reference(SMOP__Object* interpreter,
                                       SMOP__ResponderInterface* responder,
                                       SMOP__Object* obj) {
  return obj;
}

void smop_native_bool_init() {

  SMOP__NATIVE__bool = calloc(sizeof(SMOP__ResponderInterface));
  assert(SMOP__NATIVE__bool);
  SMOP__NATIVE__bool->MESSAGE = bool_message;
  SMOP__NATIVE__bool->REFERENCE = bool_reference;
  SMOP__NATIVE__bool->RELEASE = bool_release;

  smop_native_bool_const_true = calloc(sizeof(SMOP__Object));
  assert(smop_native_bool_const_true);
  smop_native_bool_const_true->RI = SMOP__NATIVE__bool;
  smop_native_bool_const_true->data = (void*)1;

  smop_native_bool_const_false = calloc(sizeof(SMOP__Object));
  assert(smop_native_bool_const_false);
  smop_native_bool_const_false->RI = SMOP__NATIVE__bool;
  smop_native_bool_const_false->data = NULL;
  
}

void smop_native_bool_destr() {
  free(smop_native_bool_const_false);
  free(somp_native_bool_const_true);
  free(SMOP__NATIVE__bool);
}


SMOP__Object* SMOP__NATIVE__bool_create(int b) {
  if (b) {
    return smop_native_bool_const_true;
  } else {
    return smop_native_bool_const_false;
  }
}

int SMOP__NATIVE__bool_fetch(SMOP__Object* v) {
  if (v->data) {
    return 1;
  } else {
    return 0;
  }
}
