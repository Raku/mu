#include <smop/util.h>

smop_util_hash* smop_util_hash_create(SMOP__Object* interpreter,int size) {
  smop_util_hash* ret = malloc(sizeof(smop_util_hash));
  printf("Created an inner hash <%p>\n",ret);
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
      smop_util_hash_bucket* next = bucket->next;
      free(bucket);
      bucket = next;
    }
  }
  free(hash->content);
  printf("Destroyed an inner hash <%p>\n",hash);
  free(hash);
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
