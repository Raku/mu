#include "yap6.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

YAP6__CORE__Hash__ProxyScalar* yap6_hash_proxyscalar_create() {
  YAP6__CORE__Hash__ProxyScalar* foo = (YAP6__CORE__Hash__ProxyScalar*)yap6_value_alloc(sizeof(YAP6__CORE__Hash__ProxyScalar));
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_hash_proxyscalar_dispatcher);
  foo->dispatcher = (YAP6__CORE__ScalarDispatcher*)yap6_const_hash_proxyscalar_dispatcher;
  return foo;
}



static void hash_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
  YAP6__CORE__Hash* hash = (YAP6__CORE__Hash*)value;
  int length = hash->length;
  YAP6__CORE__Pair** pairs = hash->pairs;
  hash->length = 0;
  hash->pairs = NULL;
  yap6_value_unlock(value);
  int i;
  for (i = 0; i < length; i++) {
    if (pairs[i]) {
      yap6_value_refcnt_dec(pairs[i]);
      pairs[i] = NULL;
    }
  }
}

static YAP6__CORE__Scalar* hash_dispatcher_LOOKP(YAP6__CORE__Dispatcher* self,
                                                 YAP6__CORE__Value* v_value,
                                                 YAP6__CORE__Value* key) {
}

static YAP6__CORE__Scalar* hash_dispatcher_EXIST(YAP6__CORE__Dispatcher* self,
                                                 YAP6__CORE__Value* v_value,
                                                 YAP6__CORE__Value* key) {
}

static YAP6__CORE__Scalar* hash_dispatcher_DELET(YAP6__CORE__Dispatcher* self,
                                                 YAP6__CORE__Value* v_value,
                                                 YAP6__CORE__Value* key) {
}

static YAP6__CORE__bytes* hash_dispatcher_WHICH(YAP6__CORE__Dispatcher* self,
                                   YAP6__CORE__Value* value) {
  char str[32];
  sprintf(str, "hash:%p", value);
  int len = strlen(str);
  return yap6_bytes_create(str, len);

}


static YAP6__CORE__int* hash_dispatcher_ELEMS(YAP6__CORE__Dispatcher* self,
                                   YAP6__CORE__Value* value) {
  yap6_value_rdlock(value);
  int length = ((YAP6__CORE__Hash*)value)->length;
  yap6_value_unlock(value);
  return yap6_int_crate(length);

}



static void hash_proxyscalar_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
  YAP6__CORE__Value* cell = ((YAP6__CORE__Scalar*)value)->cell;
  ((YAP6__CORE__Scalar*)value)->cell = NULL;
  yap6_value_unlock(value);
  if (cell) {
    yap6_value_refcnt_dec(cell);
  }
}


static YAP6__CORE__Value* hash_proxyscalar_dispatcher_FETCH(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* wants) {
  yap6_value_wrlock(value);
  YAP6__CORE__Value* val = ((YAP6__CORE__Scalar*)value)->cell;
  yap6_value_unlock(value);
  yap6_value_refcnt_inc(val);
  return val;
}

static YAP6__CORE__Value* hash_proxyscalar_dispatcher_STORE(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* newvalue) {
}

static YAP6__CORE__bytes* hash_proxyscalar_dispatcher_WHICH(YAP6__CORE__Dispatcher* self,
                                   YAP6__CORE__Value* value) {
  yap6_value_rdlock(value);
  YAP6__CORE__Value* val = ((YAP6__CORE__Scalar*)value)->cell;
  yap6_value_unlock(value);
  return YAP6_WHICH(val);
  
}


YAP6__CORE__HashDispatcher* yap6_const_hash_dispatcher;
YAP6__CORE__ScalarDispatcher* yap6_const_hash_proxyscalar_dispatcher;

void yap6_hash_dispatcher_init() {

}

YAP6__CORE__Hash* yap6_hash_create() {
}

void yap6_hash_dispatcher_destr() {
}
