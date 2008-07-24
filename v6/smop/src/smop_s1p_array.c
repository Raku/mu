#include <stdlib.h>
#include <smop.h>
#include <math.h>
#include <smop_lowlevel.h>

SMOP__Object* SMOP__S1P__Array;
static SMOP__Object* SMOP__S1P__ArrayProxy;

typedef struct smop_s1p_array_struct {
  SMOP__Object__BASE
  unsigned int elems;
  unsigned int size;
  SMOP__Object** content;
} smop_s1p_array_struct;
typedef struct smop_s1p_array_proxy_struct {
  SMOP__Object__BASE
  smop_s1p_array_struct* array;
  int index;
} smop_s1p_array_proxy_struct;

SMOP__Object* SMOP__S1P__Array_create(void) {
    smop_s1p_array_struct* ret = (smop_s1p_array_struct*) smop_lowlevel_alloc(sizeof(smop_s1p_array_struct));
    ret->RI = (SMOP__ResponderInterface*)SMOP__S1P__Array;
    ret->size = 0;
    ret->elems = 0;
    ret->content = NULL;
    return (SMOP__Object*) ret;
}

static int floor_log2(unsigned int n) {
  int pos = 0;
  if (n >= 1<<16) { n >>= 16; pos += 16; }
  if (n >= 1<< 8) { n >>=  8; pos +=  8; }
  if (n >= 1<< 4) { n >>=  4; pos +=  4; }
  if (n >= 1<< 2) { n >>=  2; pos +=  2; }
  if (n >= 1<< 1) {           pos +=  1; }
  return ((n == 0) ? (-1) : pos);
}
static void resize_array(smop_s1p_array_struct* array,int length) {
  if (length > array->size) {
    int old_size = array->size;
    array->size = 2<<(floor_log2(array->size)+1);
    array->content = realloc(array->content,array->size);
    int i;for (i=0;i<old_size;i++) array->content[i] = NULL;
  }
}
static SMOP__Object* smop_s1p_array_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  smop_s1p_array_struct* invocant = (smop_s1p_array_struct*)(SMOP__NATIVE__capture_invocant(interpreter, capture));
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  if (identifier == SMOP__ID__new) {
  } else if (identifier == SMOP__ID__postcircumfix_curly) {
    int i = SMOP__NATIVE__int_fetch(SMOP__NATIVE__capture_positional(interpreter, capture, 0));
    smop_s1p_array_proxy_struct* proxy = (smop_s1p_array_proxy_struct*) smop_lowlevel_alloc(sizeof(smop_s1p_array_proxy_struct));
    proxy->RI = (SMOP__ResponderInterface*)SMOP__S1P__ArrayProxy;
    proxy->index = i;
    proxy->array = invocant;
    ret = (SMOP__Object*) proxy;
  } else if (identifier == SMOP__ID__elems) {
    return SMOP__NATIVE__int_create(invocant->elems);
  }
  SMOP_RELEASE(interpreter,capture);
  return ret;
}
static SMOP__Object* smop_s1p_array_proxy_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  smop_s1p_array_proxy_struct* invocant = (smop_s1p_array_proxy_struct*)(SMOP__NATIVE__capture_invocant(interpreter, capture));
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  SMOP_RELEASE(interpreter,capture);
  if (identifier == SMOP__ID__FETCH) {
    if (invocant->index < invocant->array->elems && invocant->array->content[invocant->index]) {
      ret = invocant->array->content[invocant->index];
    } else {
    }
  } else if (identifier == SMOP__ID__STORE) {
    resize_array(invocant->array,invocant->index+1);
    invocant->array->content[invocant->index] = SMOP__NATIVE__capture_positional(interpreter, capture,0);
  }
  return ret;
}

void smop_s1p_array_init() {
  SMOP__S1P__Array = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->MESSAGE = smop_s1p_array_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->id = "Lowlevel array";

  SMOP__S1P__ArrayProxy = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->MESSAGE = smop_s1p_array_proxy_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->id = "Lowlevel array proxy";
}

void smop_s1p_array_destr() {
  free(SMOP__S1P__Array);
}


