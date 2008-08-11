#include <stdlib.h>
#include <smop.h>
#include <math.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>
#include <stdio.h>

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
static void resize_array(smop_s1p_array_struct* array,int new_size) {
  if (new_size > array->size) {
    int old_size = array->size;
    array->size = 1<<(floor_log2(new_size));
    //printf("new size:%d resizing to %d previous size:%d\n",new_size,array->size,old_size);
    array->content = realloc(array->content,array->size);
    int i;for (i=old_size;i<array->size;i++) array->content[i] = NULL;
  }
}
static SMOP__Object* smop_s1p_array_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  ___NATIVE_CAPTURE_ONLY___;
  ___CONST_IDENTIFIER_ONLY___;
  smop_s1p_array_struct* invocant = (smop_s1p_array_struct*)(SMOP__NATIVE__capture_invocant(interpreter, capture));
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  if (identifier == SMOP__ID__new) {
    ret = SMOP__S1P__Array_create();
  } else if (identifier == SMOP__ID__postcircumfix_square) {
    SMOP__Object* pos0 = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
    int i = SMOP__NATIVE__int_fetch(pos0);
    SMOP_RELEASE(interpreter,pos0);
    smop_s1p_array_proxy_struct* proxy = (smop_s1p_array_proxy_struct*) smop_lowlevel_alloc(sizeof(smop_s1p_array_proxy_struct));
    proxy->RI = (SMOP__ResponderInterface*)SMOP__S1P__ArrayProxy;
    proxy->index = i;
    proxy->array = SMOP_REFERENCE(interpreter,invocant);
    ret = (SMOP__Object*) proxy;
  } else if (identifier == SMOP__ID__elems) {
    ret = SMOP__NATIVE__int_create(invocant->elems);
  } else if (identifier == SMOP__ID__DESTROYALL) {
    int i;for (i=0;i < invocant->elems;i++) {
      if (invocant->content[i]) SMOP_RELEASE(interpreter,invocant->content[i]);
    }
    free(invocant->content);
  } else {
      ___UNKNOWN_METHOD___
  }
  SMOP_RELEASE(interpreter,invocant);
  SMOP_RELEASE(interpreter,capture);
  return ret;
}
static SMOP__Object* smop_s1p_array_proxy_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  smop_s1p_array_proxy_struct* invocant = (smop_s1p_array_proxy_struct*)(SMOP__NATIVE__capture_invocant(interpreter, capture));
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  if (identifier == SMOP__ID__FETCH) {
    if (invocant->index < invocant->array->elems && invocant->array->content[invocant->index]) {
      ret = SMOP_REFERENCE(interpreter,invocant->array->content[invocant->index]);
    } else {
    }
  } else if (identifier == SMOP__ID__STORE) {
    resize_array(invocant->array,invocant->index+1);
    SMOP__Object* prev = invocant->array->content[invocant->index];
    if (prev) SMOP_RELEASE(interpreter,prev);
    invocant->array->content[invocant->index] = SMOP__NATIVE__capture_positional(interpreter, capture,0);
    if (invocant->array->elems <= invocant->index) invocant->array->elems = invocant->index+1;
  } else if (identifier == SMOP__ID__DESTROYALL) {
    SMOP_RELEASE(interpreter,invocant->array);
  } else {
      ___UNKNOWN_METHOD___
  }
  SMOP_RELEASE(interpreter,invocant);
  SMOP_RELEASE(interpreter,capture);
  return ret;
}

void smop_s1p_array_init() {
  SMOP__S1P__Array = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->MESSAGE = smop_s1p_array_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Array)->id = "Lowlevel array";

  SMOP__S1P__ArrayProxy = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__ArrayProxy)->MESSAGE = smop_s1p_array_proxy_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__ArrayProxy)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__ArrayProxy)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__ArrayProxy)->id = "Lowlevel array proxy";
}

void smop_s1p_array_destr() {
  free(SMOP__S1P__Array);
  free(SMOP__S1P__ArrayProxy);
}


