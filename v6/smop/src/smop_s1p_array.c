#include <stdlib.h>
#include <smop.h>
#include <smop_lowlevel.h>

SMOP__Object* SMOP__LOWLEVEL__array;

typedef struct smop_s1p_array_struct {
  SMOP__Object__BASE
  int size;
  SMOP__Object** content;
} smop_s1p_array_struct;

static SMOP__Object* smop_s1p_array_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  smop_s1p_array_struct* invocant = (smop_s1p_array_struct*)(SMOP__NATIVE__capture_invocant(interpreter, capture));
  SMOP_RELEASE(interpreter,capture);
  return SMOP__NATIVE__bool_false;
}


void smop_s1p_array_init() {
  SMOP__LOWLEVEL__array = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__LOWLEVEL__array)->MESSAGE = smop_s1p_array_message;
  ((SMOP__ResponderInterface*)SMOP__LOWLEVEL__array)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__LOWLEVEL__array)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__LOWLEVEL__array)->id = "Lowlevel array";
}

void smop_s1p_array_destr() {
  free(SMOP__LOWLEVEL__array);
}


