#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <smop_base.h>

SMOP__Object* SMOP__NATIVE__bool;
SMOP__Object* SMOP__NATIVE__bool_true;
SMOP__Object* SMOP__NATIVE__bool_false;

static SMOP__Object* bool_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  fprintf(stderr,"bool MESSAGE not loaded yet\n");
  abort();
}

static SMOP__Object* bool_reference(SMOP__Object* interpreter,
                                    SMOP__ResponderInterface* responder,
                                    SMOP__Object* obj) {
  return obj;
}

static SMOP__Object* bool_release(SMOP__Object* interpreter,
                                  SMOP__ResponderInterface* responder,
                                  SMOP__Object* obj) {
  return obj;
}

void smop_native_bool_init() {

  SMOP__NATIVE__bool = calloc(1,sizeof(SMOP__ResponderInterface));
  SMOP__ResponderInterface* ri = (SMOP__ResponderInterface*)SMOP__NATIVE__bool;
  assert(SMOP__NATIVE__bool);
  ri->MESSAGE = bool_message;
  ri->REFERENCE = bool_reference;
  ri->RELEASE = bool_release;
  ri->id = "Native Boolean";

  SMOP__NATIVE__bool_true = calloc(1,sizeof(SMOP__Object));
  assert(SMOP__NATIVE__bool_true);
  SMOP__NATIVE__bool_true->RI = ri;

  SMOP__NATIVE__bool_false = calloc(1,sizeof(SMOP__Object));
  assert(SMOP__NATIVE__bool_false);
  SMOP__NATIVE__bool_false->RI = ri;
  
}

void smop_native_bool_destr() {
  free(SMOP__NATIVE__bool_false);
  free(SMOP__NATIVE__bool_true);
  free(SMOP__NATIVE__bool);
}
