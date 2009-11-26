#include <smop/util.h>

smop_util_hash* smop_util_hash_create(SMOP__Object* interpreter,int size) {
  smop_util_hash* ret = malloc(sizeof(smop_util_hash));
  ret->size = size;
  ret->content = calloc(ret->size,sizeof(smop_util_hash_bucket*));
  return ret;
}

void smop_util_hash_destr(SMOP__Object* interpreter,smop_util_hash* hash) {
  int i;
  for (i=0;i < hash->size;i++) {
    smop_util_hash_bucket* bucket = hash->content[i];
    while (bucket) {
      SMOP_RELEASE(interpreter,bucket->value);
      SMOP_RELEASE(interpreter,bucket->key);
      smop_util_hash_bucket* next = bucket->next;
      free(bucket);
      bucket = next;
    }
  }
  free(hash->content);
  free(hash);
}
smop_util_hash* smop_util_hash_copy(SMOP__Object* interpreter,smop_util_hash* hash) {
  smop_util_hash* ret = malloc(sizeof(smop_util_hash));
  ret->size = hash->size;
  ret->content = calloc(ret->size,sizeof(smop_util_hash_bucket*));
  int i;
  for (i=0;i < hash->size;i++) {
    smop_util_hash_bucket* bucket = hash->content[i];
    smop_util_hash_bucket* new_bucket = bucket ? (smop_util_hash_bucket*) malloc(sizeof(smop_util_hash_bucket)) : NULL;
    ret->content[i] = new_bucket;
    while (bucket) {
      new_bucket->value = SMOP_REFERENCE(interpreter,bucket->value);
      new_bucket->key = SMOP_REFERENCE(interpreter,bucket->key);
      smop_util_hash_bucket* next = bucket->next;
      if (next) {
       smop_util_hash_bucket* new_next = (smop_util_hash_bucket*) malloc(sizeof(smop_util_hash_bucket));
       new_bucket->next = new_next;
       new_bucket = new_next;
      }
      bucket = next;
    }
    if (new_bucket) new_bucket->next = NULL;
  }
  return ret;
}

void smop_util_hash_set(SMOP__Object* interpreter,smop_util_hash* hash,SMOP__Object* key,SMOP__Object* value) {
  int hash_value = 0;
  smop_util_hash_bucket* bucket = hash->content[hash_value % hash->size];
  while (bucket) {
    if (bucket->key == key) {
      SMOP_RELEASE(interpreter,bucket->value);
      bucket->value = value;
      return;
    }
    bucket = bucket->next;
  } 
  smop_util_hash_bucket* new_bucket = (smop_util_hash_bucket*) malloc(sizeof(smop_util_hash_bucket));
  new_bucket->next = hash->content[hash_value % hash->size];
  new_bucket->key = key;
  new_bucket->value = value;
  hash->content[hash_value % hash->size] = new_bucket;
}

SMOP__Object* smop_util_hash_get(SMOP__Object* interpreter,smop_util_hash* hash,SMOP__Object* key) {
  int hash_value = 0;
  smop_util_hash_bucket* bucket = hash->content[hash_value % hash->size];
  while (bucket) {
    if (bucket->key == key) return bucket->value;
    bucket = bucket->next;
  }
  return 0;
}
