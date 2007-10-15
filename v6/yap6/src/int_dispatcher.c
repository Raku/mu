#include "yap6.h"
#include <assert.h>
#include <stdlib.h>

static YAP6__CORE__Value* int_dispatcher_APPLY(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Capture* arguments,
                                          YAP6__CORE__Value* wants) {
  // TODO
  return value;
}

static void int_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
  // does nothing
}

static int int_dispatcher_COMPR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* other) {
  // TODO
  return 0;
}

YAP6__CORE__Dispatcher* yap6_const_int_dispatcher;

void yap6_int_dispatcher_init() {
  yap6_const_int_dispatcher = (YAP6__CORE__Dispatcher*)yap6_value_alloc(sizeof(YAP6__CORE__Dispatcher));
  yap6_const_int_dispatcher->APPLY = &int_dispatcher_APPLY;
  yap6_const_int_dispatcher->DESTR = &int_dispatcher_DESTR;
  yap6_const_int_dispatcher->COMPR = &int_dispatcher_COMPR;
}

YAP6__CORE__int* yap6_int_create(int lowl) {
  YAP6__CORE__int* foo = (YAP6__CORE__int*)yap6_value_alloc(sizeof(YAP6__CORE__int));
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_int_dispatcher);
  foo->dispatcher = yap6_const_int_dispatcher;
  foo->value = lowl;
  return foo;
}

int yap6_int_lowlevel(YAP6__CORE__int* val) {
  yap6_value_rdlock((YAP6__CORE__Value*)val);
  int v = val->value;
  yap6_value_unlock((YAP6__CORE__Value*)val);
  return v;
}
