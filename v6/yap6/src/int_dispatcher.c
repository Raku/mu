#include "yap6.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


static void int_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
  // does nothing
}

static YAP6__CORE__bytes* int_dispatcher_WHICH(YAP6__CORE__Dispatcher* self,
                                               YAP6__CORE__Value* value) {
  char res[16];
  yap6_value_rdlock(value);
  sprintf(res, "%d", ((YAP6__CORE__int*)value)->value);
  yap6_value_unlock(value);
  int size = strlen(res);
  return yap6_bytes_create(res, size);
  
}

static YAP6__CORE__Value* int_dispatcher_BOOLN(YAP6__CORE__Dispatcher* self,
                                               YAP6__CORE__Value* value) {
  yap6_value_rdlock(value);
  int v = ((YAP6__CORE__int*)value)->value;
  yap6_value_unlock(value);
  if (v) {
    return yap6_value_refcnt_inc(value);
  } else {
    return yap6_value_refcnt_inc(yap6_bool_false);
  }
}

YAP6__CORE__Dispatcher* yap6_const_int_dispatcher;

void yap6_int_dispatcher_init() {
  yap6_const_int_dispatcher = (YAP6__CORE__Dispatcher*)yap6_value_alloc(sizeof(YAP6__CORE__Dispatcher));
  yap6_const_int_dispatcher->DESTR = &int_dispatcher_DESTR;
  yap6_const_int_dispatcher->BOOLN = &int_dispatcher_BOOLN;
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

void yap6_int_dispatcher_destr() {
  yap6_value_refcnt_dec((YAP6__CORE__Value*)yap6_const_int_dispatcher);
}

void yap6_int_dispatcher_which_init() {
  yap6_const_int_dispatcher->WHICH = &int_dispatcher_WHICH;
}
