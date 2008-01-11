#include "smop.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

SMOP__CORE__Hash__ProxyScalar* smop_hash_proxyscalar_create() {
  SMOP__CORE__Hash__ProxyScalar* foo = (SMOP__CORE__Hash__ProxyScalar*)smop_value_alloc(sizeof(SMOP__CORE__Hash__ProxyScalar));
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_hash_proxyscalar_dispatcher);
  foo->dispatcher = (SMOP__CORE__ScalarDispatcher*)smop_const_hash_proxyscalar_dispatcher;
  return foo;
}



static void hash_dispatcher_DESTR(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value) {
  smop_value_wrlock(value);
  SMOP__CORE__Hash* hash = (SMOP__CORE__Hash*)value;
  int length = hash->length;
  SMOP__CORE__Pair** pairs = hash->pairs;
  hash->length = 0;
  hash->pairs = NULL;
  smop_value_unlock(value);
  int i;
  for (i = 0; i < length; i++) {
    if (pairs[i]) {
      smop_value_refcnt_dec(pairs[i]);
      pairs[i] = NULL;
    }
  }
}

static SMOP__CORE__Scalar* hash_dispatcher_LOOKP(SMOP__CORE__Dispatcher* self,
                                                 SMOP__CORE__Value* v_value,
                                                 SMOP__CORE__Value* key) {
}

static SMOP__CORE__Scalar* hash_dispatcher_EXIST(SMOP__CORE__Dispatcher* self,
                                                 SMOP__CORE__Value* v_value,
                                                 SMOP__CORE__Value* key) {
}

static SMOP__CORE__Scalar* hash_dispatcher_DELET(SMOP__CORE__Dispatcher* self,
                                                 SMOP__CORE__Value* v_value,
                                                 SMOP__CORE__Value* key) {
}

static SMOP__CORE__bytes* hash_dispatcher_WHICH(SMOP__CORE__Dispatcher* self,
                                   SMOP__CORE__Value* value) {
  char str[32];
  sprintf(str, "hash:%p", value);
  int len = strlen(str);
  return smop_bytes_create(str, len);

}


static SMOP__CORE__int* hash_dispatcher_ELEMS(SMOP__CORE__Dispatcher* self,
                                   SMOP__CORE__Value* value) {
  smop_value_rdlock(value);
  int length = ((SMOP__CORE__Hash*)value)->length;
  smop_value_unlock(value);
  return smop_int_crate(length);

}



static void hash_proxyscalar_dispatcher_DESTR(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value) {
  smop_value_wrlock(value);
  SMOP__CORE__Value* cell = ((SMOP__CORE__Scalar*)value)->cell;
  ((SMOP__CORE__Scalar*)value)->cell = NULL;
  smop_value_unlock(value);
  if (cell) {
    smop_value_refcnt_dec(cell);
  }
}


static SMOP__CORE__Value* hash_proxyscalar_dispatcher_FETCH(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value,
                                          SMOP__CORE__Value* wants) {
  smop_value_wrlock(value);
  SMOP__CORE__Value* val = ((SMOP__CORE__Scalar*)value)->cell;
  smop_value_unlock(value);
  smop_value_refcnt_inc(val);
  return val;
}

static SMOP__CORE__Value* hash_proxyscalar_dispatcher_STORE(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value,
                                          SMOP__CORE__Value* newvalue) {
}

static SMOP__CORE__bytes* hash_proxyscalar_dispatcher_WHICH(SMOP__CORE__Dispatcher* self,
                                   SMOP__CORE__Value* value) {
  smop_value_rdlock(value);
  SMOP__CORE__Value* val = ((SMOP__CORE__Scalar*)value)->cell;
  smop_value_unlock(value);
  return SMOP_WHICH(val);
  
}


SMOP__CORE__HashDispatcher* smop_const_hash_dispatcher;
SMOP__CORE__ScalarDispatcher* smop_const_hash_proxyscalar_dispatcher;

void smop_hash_dispatcher_init() {

}

SMOP__CORE__Hash* smop_hash_create() {
}

void smop_hash_dispatcher_destr() {
}
