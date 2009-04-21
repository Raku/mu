#include <stdlib.h>
#include <smop/base.h>
#include <smop/nagc.h>
#include <smop/s0native.h>
#include <smop/util.h>

SMOP__NAGC__ResponderInterface* SMOP__capture__RI;


typedef struct capture_struct {
  SMOP__NAGC__Object__BASE
  int positional_count;
  SMOP__Object** positional;
  smop_util_hash* named;
} capture_struct;


static void DESTROYALL(SMOP__Object* interpreter,
                              SMOP__Object* value) {
  capture_struct* c = (capture_struct*) value;
  int i;
  for (i=0;i<c->positional_count;i++) {
    SMOP_RELEASE(interpreter,c->positional[i]);
  }
  free(c->positional);
  smop_util_hash_destr(interpreter,c->named);
}

SMOP__Object* SMOP__NATIVE__capture_create(SMOP__Object* interpreter,SMOP__Object** positional,SMOP__Object** named) {
  capture_struct* ret = (capture_struct*) smop_nagc_alloc(sizeof(capture_struct));
  ret->RI = (SMOP__ResponderInterface*)SMOP__capture__RI;

  ret->named = smop_util_hash_create(interpreter,20);

  SMOP__Object** p = positional;
  while (*p) p++;
  ret->positional_count = (p-positional);

  ret->positional = malloc(sizeof(SMOP__Object*) * ret->positional_count);
  int i;
  for (i=0;i<ret->positional_count;i++) {
    ret->positional[i] = positional[i];
  }

  SMOP__Object** n = named;
  while (*n) {
    smop_util_hash_set(interpreter,ret->named,*n,*(n+1));
    n += 2;
  }

  return (SMOP__Object*) ret;
}

SMOP__Object* SMOP__NATIVE__capture_named(SMOP__Object* interpreter,SMOP__Object* capture,SMOP__Object* key,int i) {
  SMOP__Object* ret = smop_util_hash_get(interpreter,((capture_struct*)capture)->named,key);
  if (!ret) ret = SMOP__NATIVE__bool_false;
  return SMOP_REFERENCE(interpreter,ret);
}

SMOP__Object* SMOP__NATIVE__capture_positional(SMOP__Object* interpreter,SMOP__Object* capture,int i) {
  if (i < ((capture_struct*)capture)->positional_count) {
    return SMOP_REFERENCE(interpreter,((capture_struct*)capture)->positional[i]);
  } else {
    printf("positional_count = %d i = %d\n",((capture_struct*)capture)->positional_count,i);
    abort();
  }
}

int SMOP__NATIVE__capture_positional_count(SMOP__Object* interpreter,SMOP__Object* capture) {
  return ((capture_struct*)capture)->positional_count;
}

void smop_capture_init() {
  SMOP__capture__RI = malloc(sizeof(SMOP__NAGC__ResponderInterface));
  SMOP__capture__RI->MESSAGE = smop_placeholder_message;
  SMOP__capture__RI->REFERENCE = smop_nagc_reference;
  SMOP__capture__RI->RELEASE = smop_nagc_release;
  SMOP__capture__RI->WEAKREF = smop_nagc_weakref;
  SMOP__capture__RI->id = "capture";
  SMOP__capture__RI->DESTROYALL = DESTROYALL;
}

void smop_capture_destr() {
  free(SMOP__capture__RI);
}
