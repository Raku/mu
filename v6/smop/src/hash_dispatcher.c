#include "vroom.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

VROOM__CORE__Hash__ProxyScalar* vroom_hash_proxyscalar_create() {
  VROOM__CORE__Hash__ProxyScalar* foo = (VROOM__CORE__Hash__ProxyScalar*)vroom_value_alloc(sizeof(VROOM__CORE__Hash__ProxyScalar));
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_hash_proxyscalar_dispatcher);
  foo->dispatcher = (VROOM__CORE__ScalarDispatcher*)vroom_const_hash_proxyscalar_dispatcher;
  return foo;
}



static void hash_dispatcher_DESTR(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value) {
  vroom_value_wrlock(value);
  VROOM__CORE__Hash* hash = (VROOM__CORE__Hash*)value;
  int length = hash->length;
  VROOM__CORE__Pair** pairs = hash->pairs;
  hash->length = 0;
  hash->pairs = NULL;
  vroom_value_unlock(value);
  int i;
  for (i = 0; i < length; i++) {
    if (pairs[i]) {
      vroom_value_refcnt_dec(pairs[i]);
      pairs[i] = NULL;
    }
  }
}

static VROOM__CORE__Scalar* hash_dispatcher_LOOKP(VROOM__CORE__Dispatcher* self,
                                                 VROOM__CORE__Value* v_value,
                                                 VROOM__CORE__Value* key) {
}

static VROOM__CORE__Scalar* hash_dispatcher_EXIST(VROOM__CORE__Dispatcher* self,
                                                 VROOM__CORE__Value* v_value,
                                                 VROOM__CORE__Value* key) {
}

static VROOM__CORE__Scalar* hash_dispatcher_DELET(VROOM__CORE__Dispatcher* self,
                                                 VROOM__CORE__Value* v_value,
                                                 VROOM__CORE__Value* key) {
}

static VROOM__CORE__bytes* hash_dispatcher_WHICH(VROOM__CORE__Dispatcher* self,
                                   VROOM__CORE__Value* value) {
  char str[32];
  sprintf(str, "hash:%p", value);
  int len = strlen(str);
  return vroom_bytes_create(str, len);

}


static VROOM__CORE__int* hash_dispatcher_ELEMS(VROOM__CORE__Dispatcher* self,
                                   VROOM__CORE__Value* value) {
  vroom_value_rdlock(value);
  int length = ((VROOM__CORE__Hash*)value)->length;
  vroom_value_unlock(value);
  return vroom_int_crate(length);

}



static void hash_proxyscalar_dispatcher_DESTR(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value) {
  vroom_value_wrlock(value);
  VROOM__CORE__Value* cell = ((VROOM__CORE__Scalar*)value)->cell;
  ((VROOM__CORE__Scalar*)value)->cell = NULL;
  vroom_value_unlock(value);
  if (cell) {
    vroom_value_refcnt_dec(cell);
  }
}


static VROOM__CORE__Value* hash_proxyscalar_dispatcher_FETCH(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value,
                                          VROOM__CORE__Value* wants) {
  vroom_value_wrlock(value);
  VROOM__CORE__Value* val = ((VROOM__CORE__Scalar*)value)->cell;
  vroom_value_unlock(value);
  vroom_value_refcnt_inc(val);
  return val;
}

static VROOM__CORE__Value* hash_proxyscalar_dispatcher_STORE(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value,
                                          VROOM__CORE__Value* newvalue) {
}

static VROOM__CORE__bytes* hash_proxyscalar_dispatcher_WHICH(VROOM__CORE__Dispatcher* self,
                                   VROOM__CORE__Value* value) {
  vroom_value_rdlock(value);
  VROOM__CORE__Value* val = ((VROOM__CORE__Scalar*)value)->cell;
  vroom_value_unlock(value);
  return VROOM_WHICH(val);
  
}


VROOM__CORE__HashDispatcher* vroom_const_hash_dispatcher;
VROOM__CORE__ScalarDispatcher* vroom_const_hash_proxyscalar_dispatcher;

void vroom_hash_dispatcher_init() {

}

VROOM__CORE__Hash* vroom_hash_create() {
}

void vroom_hash_dispatcher_destr() {
}
