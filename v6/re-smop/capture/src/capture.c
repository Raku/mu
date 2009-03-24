#include <stdlib.h>
#include <smop/base.h>
#include <smop/nagc.h>
#include <smop/s0native.h>

static SMOP__NAGC__ResponderInterface* RI;

typedef struct smop_hash_bucket {
  struct smop_hash_bucket* next;
  SMOP__Object* key;
  SMOP__Object* value;
} smop_hash_bucket;

typedef struct smop_hash {
    int size;
    smop_hash_bucket** content;
} smop_hash;

typedef struct capture_struct {
  SMOP__NAGC__Object__BASE
  int positional_count;
  SMOP__Object** positional;
  smop_hash* named;
} capture_struct;

smop_hash* smop_hash_create(SMOP__Object* interpreter,int size) {
  smop_hash* ret = malloc(sizeof(smop_hash));
  ret->size = size;
  ret->content = calloc(ret->size,sizeof(smop_hash_bucket*));
  return ret;
}

void smop_hash_set(SMOP__Object* interpreter,smop_hash* hash,SMOP__Object* key,SMOP__Object* value) {
  int hash_value = 0;
  smop_hash_bucket* bucket = hash->content[hash_value % hash->size];
  while (bucket) {
    if (bucket->key == key) {
      SMOP_RELEASE(interpreter,bucket->value);
      bucket->value = value;
      return;
    }
    bucket = bucket->next;
  } 
  smop_hash_bucket* new_bucket = (smop_hash_bucket*) malloc(sizeof(smop_hash_bucket));
  new_bucket->next = hash->content[hash_value % hash->size];
  new_bucket->key = key;
  new_bucket->value = value;
  hash->content[hash_value % hash->size] = new_bucket;
}

SMOP__Object* smop_hash_get(SMOP__Object* interpreter,smop_hash* hash,SMOP__Object* key) {
  int hash_value = 0;
  smop_hash_bucket* bucket = hash->content[hash_value % hash->size];
  while (bucket) {
    if (bucket->key == key) return bucket->value;
    bucket = bucket->next;
  }
  return 0;
}

static void DESTROYALL(SMOP__Object* interpreter,
                              SMOP__Object* value) {
  capture_struct* c = (capture_struct*) value;
  int i;
  for (i=0;i<c->positional_count;i++) {
    SMOP_RELEASE(interpreter,c->positional[i]);
  }
}

SMOP__Object* SMOP__NATIVE__capture_create(SMOP__Object* interpreter,SMOP__Object** positional,SMOP__Object** named) {
  capture_struct* ret = (capture_struct*) smop_nagc_alloc(sizeof(capture_struct));
  ret->RI = RI;

  ret->named = smop_hash_create(interpreter,20);

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
    smop_hash_set(interpreter,ret->named,*n,*(n+1));
    n += 2;
  }

  return (SMOP__Object*) ret;
}

SMOP__Object* SMOP__NATIVE__capture_named(SMOP__Object* interpreter,SMOP__Object* capture,SMOP__Object* key,int i) {
  return smop_hash_get(interpreter,((capture_struct*)capture)->named,key);
}

SMOP__Object* SMOP__NATIVE__capture_positional(SMOP__Object* interpreter,SMOP__Object* capture,int i) {
  return SMOP_REFERENCE(interpreter,((capture_struct*)capture)->positional[i]);
}

void smop_capture_init() {
  RI = malloc(sizeof(SMOP__NAGC__ResponderInterface));
  RI->MESSAGE = smop_placeholder_message;
  RI->REFERENCE = smop_nagc_reference;
  RI->RELEASE = smop_nagc_release;
  RI->WEAKREF = smop_nagc_weakref;
  RI->id = "Native Capture";
  RI->DESTROYALL = DESTROYALL;
}

void smop_capture_destr() {
  free(RI);
}
