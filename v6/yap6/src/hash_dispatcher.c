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


static YAP6__CORE__Value* hash_dispatcher_APPLY(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__List* arguments,
                                          YAP6__CORE__Value* wants) {
}


static void hash_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
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

static YAP6__CORE__Value* hash_proxyscalar_dispatcher_APPLY(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__List* arguments,
                                          YAP6__CORE__Value* wants) {
}

static void hash_proxyscalar_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
}


static YAP6__CORE__Value* hash_proxyscalar_dispatcher_FETCH(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* wants) {
}

static YAP6__CORE__Value* hash_proxyscalar_dispatcher_STORE(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* newvalue) {
}

static YAP6__CORE__bytes* hash_proxyscalar_dispatcher_WHICH(YAP6__CORE__Dispatcher* self,
                                   YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
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
