#include <stdlib.h>
#include <smop/base.h>
#include <smop/nagc.h>
#include <smop/s0native.h>
#include <smop/util.h>
#include <smop/capture.h>

SMOP__Object* SMOP__capture__RI;


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

static SMOP__Object* DUMP(SMOP__Object* interpreter,
                                SMOP__ResponderInterface* responder,
                                SMOP__Object* obj) {

  return smop_dump_create((SMOP__Object*[]) {
      SMOP_DUMP_NAGC,
      NULL
  });
}

SMOP__Object* SMOP__NATIVE__capture_create(SMOP__Object* interpreter,SMOP__Object** positional,SMOP__Object** named) {
  capture_struct* ret = (capture_struct*) smop_nagc_alloc(sizeof(capture_struct));
  ret->RI = (SMOP__ResponderInterface*)SMOP__capture__RI;

  ret->named = smop_util_hash_create(interpreter,1);

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

SMOP__Object* SMOP__NATIVE__capture_create_from_capture(SMOP__Object* interpreter,SMOP__Object* capture) {
  capture_struct* ret = (capture_struct*) smop_nagc_alloc(sizeof(capture_struct));
  capture_struct* from = (capture_struct*) capture;
  ret->RI = (SMOP__ResponderInterface*)SMOP__capture__RI;

  ret->named = smop_util_hash_copy(interpreter,from->named);

  int count = SMOP__NATIVE__capture_positional_count(interpreter,capture);
  SMOP__Object** pos = malloc(sizeof(SMOP__Object*) * (count-1));
  int i;
  for (i=1;i<count;i++) pos[i-1] = SMOP_REFERENCE(interpreter,from->positional[i]);
  ret->positional_count = count-1;
  ret->positional = pos;
  return (SMOP__Object*) ret;
}

SMOP__Object* SMOP__NATIVE__capture_delegate(SMOP__Object* interpreter,SMOP__Object* capture,SMOP__Object* invocant) {
  capture_struct* ret = (capture_struct*) smop_nagc_alloc(sizeof(capture_struct));
  capture_struct* from = (capture_struct*) capture;
  ret->RI = (SMOP__ResponderInterface*)SMOP__capture__RI;

  ret->named = smop_util_hash_copy(interpreter,from->named);

  int count = SMOP__NATIVE__capture_positional_count(interpreter,capture);
  SMOP__Object** pos = malloc(sizeof(SMOP__Object*) * count);
  int i;
  pos[0] = invocant;
  for (i=1;i<count;i++) pos[i] = SMOP_REFERENCE(interpreter,from->positional[i]);
  ret->positional_count = count;
  ret->positional = pos;
  return (SMOP__Object*) ret;
}

SMOP__Object* SMOP__NATIVE__capture_named(SMOP__Object* interpreter,SMOP__Object* capture,SMOP__Object* key) {
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
int SMOP__NATIVE__capture_named_count(SMOP__Object* interpreter,SMOP__Object* capture) {
  smop_util_hash* hash = ((capture_struct*)capture)->named;

  int i;
  int bucket_count = 0;
  for (i=0;i < hash->size;i++) {
    smop_util_hash_bucket* bucket = hash->content[i];
    while (bucket) {
      bucket_count++;
      bucket = bucket->next;
    }
  }
  return bucket_count;
}


void smop_capture_init() {
  SMOP__capture__RI = malloc(sizeof(SMOP__NAGC__ResponderInterface));
  ((SMOP__NAGC__ResponderInterface*)SMOP__capture__RI)->RI = SMOP__metaRI;
  ((SMOP__NAGC__ResponderInterface*)SMOP__capture__RI)->MESSAGE = smop_placeholder_message;
  ((SMOP__NAGC__ResponderInterface*)SMOP__capture__RI)->REFERENCE = smop_nagc_reference;
  ((SMOP__NAGC__ResponderInterface*)SMOP__capture__RI)->RELEASE = smop_nagc_release;
  ((SMOP__NAGC__ResponderInterface*)SMOP__capture__RI)->WEAKREF = smop_nagc_weakref;
  ((SMOP__NAGC__ResponderInterface*)SMOP__capture__RI)->id = "capture";
  ((SMOP__NAGC__ResponderInterface*)SMOP__capture__RI)->DESTROYALL = DESTROYALL;
  ((SMOP__NAGC__ResponderInterface*)SMOP__capture__RI)->DUMP = DUMP;
}

void smop_capture_destr() {
  free(SMOP__capture__RI);
}
