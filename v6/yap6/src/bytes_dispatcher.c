#include "yap6.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static YAP6__CORE__Value* bytes_dispatcher_APPLY(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Capture* arguments,
                                          YAP6__CORE__Value* wants) {
  // TODO
  return value;
}

static void bytes_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
  YAP6__CORE__bytes* v = (YAP6__CORE__bytes*)value;
  free(v->value);
  v->value = NULL;
  yap6_value_unlock(value);
}

static YAP6__CORE__bytes* bytes_dispatcher_WHICH(YAP6__CORE__Dispatcher* self,
                                               YAP6__CORE__Value* value) {
  return value;
}

YAP6__CORE__Dispatcher* yap6_const_bytes_dispatcher;

void yap6_bytes_dispatcher_init() {
  yap6_const_bytes_dispatcher = (YAP6__CORE__Dispatcher*)yap6_value_alloc(sizeof(YAP6__CORE__Dispatcher));
  yap6_const_bytes_dispatcher->dispatcher = yap6_const_ident_dispatcher;
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_ident_dispatcher);
  yap6_const_bytes_dispatcher->APPLY = &bytes_dispatcher_APPLY;
  yap6_const_bytes_dispatcher->DESTR = &bytes_dispatcher_DESTR;
  yap6_const_bytes_dispatcher->WHICH = &bytes_dispatcher_WHICH;
}

YAP6__CORE__bytes* yap6_bytes_create(const char* lowl, int size) {
  YAP6__CORE__bytes* foo = (YAP6__CORE__bytes*)yap6_value_alloc(sizeof(YAP6__CORE__bytes));
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_bytes_dispatcher);
  foo->dispatcher = yap6_const_bytes_dispatcher;
  foo->value = malloc(size);
  memcpy(foo->value, lowl, size);
  foo->size = size;
  return foo;
}

char* yap6_bytes_lowlevel(YAP6__CORE__bytes* val, int* size_ret) {
  yap6_value_rdlock((YAP6__CORE__Value*)val);
  char* v = val->value;
  *size_ret = val->size;
  yap6_value_unlock((YAP6__CORE__Value*)val);
  return v;
}

void yap6_bytes_dispatcher_destr() {
  yap6_value_refcnt_dec((YAP6__CORE__Value*)yap6_const_bytes_dispatcher);
}
