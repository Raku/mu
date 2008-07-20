#include <stdlib.h>
#include <smop.h>
#include <smop_lowlevel.h>

SMOP__Object* SMOP__S1P__Array;

typedef struct smop_s1p_array_struct {
  SMOP__Object__BASE
  int elems;
  int size;
  SMOP__Object** content;
} smop_s1p_array_struct;

SMOP__Object* SMOP__S1P__Array_create(void) {
    smop_s1p_array_struct* ret = (smop_s1p_array_struct*) smop_lowlevel_alloc(sizeof(smop_s1p_array_struct));
    ret->RI = (SMOP__ResponderInterface*)SMOP__S1P__Array;
    ret->size = 0;
    ret->content = NULL;
    return (SMOP__Object*) ret;
}

static SMOP__Object* smop_s1p_array_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  smop_s1p_array_struct* invocant = (smop_s1p_array_struct*)(SMOP__NATIVE__capture_invocant(interpreter, capture));
  SMOP_RELEASE(interpreter,capture);
  return SMOP__NATIVE__bool_false;
}


void smop_s1p_array_init() {
  SMOP__S1P__Array = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->MESSAGE = smop_s1p_array_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->id = "Lowlevel array";
}

void smop_s1p_array_destr() {
  free(SMOP__S1P__Array);
}


