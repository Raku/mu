#ifndef SMOP_UTIL_H
#define SMOP_UTIL_H
#include <stdio.h>
#include <stdlib.h>
#include <smop/base.h>
#define ___VALUE_FETCH___\
        SMOP_REFERENCE(interpreter,(SMOP__Object*) invocant);\
    ret = invocant;
#define ___VALUE_STORE___ \
        { \
                fprintf(stderr, "can't call STORE on an read-only value at %s line %d file %s\n",__func__,__LINE__,__FILE__); \
                abort(); \
                } 

#define ___UNKNOWN_METHOD___ \
        { \
                int identifier_size;\
                char* s = SMOP__NATIVE__idconst_fetch(identifier,&identifier_size);\
                fprintf(stderr,"unknown method \"%.*s\" at %s line %d file %s\n",identifier_size,s,__func__,__LINE__,__FILE__); \
                abort(); \
                } 

typedef struct smop_util_hash_bucket {
  struct smop_util_hash_bucket* next;
  SMOP__Object* key;
  SMOP__Object* value;
} smop_util_hash_bucket;

typedef struct smop_util_hash {
    int size;
    smop_util_hash_bucket** content;
} smop_util_hash;

smop_util_hash* smop_util_hash_create(SMOP__Object* interpreter,int size);
void smop_util_hash_destr(SMOP__Object* interpreter,smop_util_hash* hash);
void smop_util_hash_set(SMOP__Object* interpreter,smop_util_hash* hash,SMOP__Object* key,SMOP__Object* value);
SMOP__Object* smop_util_hash_get(SMOP__Object* interpreter,smop_util_hash* hash,SMOP__Object* key);

#endif
