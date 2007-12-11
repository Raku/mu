#include "yap6.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

static YAP6__CORE__Value* scalar_dispatcher_APPLY(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__List* arguments,
                                          YAP6__CORE__Value* wants) {
  // TODO
  return NULL;
}

static void scalar_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
  YAP6__CORE__Value* cell = ((YAP6__CORE__Scalar*)value)->cell;
  ((YAP6__CORE__Scalar*)value)->cell = NULL;
  yap6_value_unlock(value);
  if (cell) {
    yap6_value_refcnt_dec(cell);
  }
}


static YAP6__CORE__Value* scalar_dispatcher_FETCH(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* wants) {

  yap6_value_wrlock(value);
  YAP6__CORE__Value* val = ((YAP6__CORE__Scalar*)value)->cell;
  yap6_value_unlock(value);
  yap6_value_refcnt_inc(val);
  return val;
}

static YAP6__CORE__Value* scalar_dispatcher_STORE(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* newvalue) {
  yap6_value_wrlock(value);
  YAP6__CORE__Value* oldval = ((YAP6__CORE__Scalar*)value)->cell;
  ((YAP6__CORE__Scalar*)value)->cell = newvalue;
  yap6_value_unlock(value);
  yap6_value_refcnt_inc(newvalue);
  return oldval;
}


YAP6__CORE__ScalarDispatcher* yap6_const_scalar_dispatcher;

void yap6_scalar_dispatcher_init() {

  yap6_const_scalar_dispatcher = (YAP6__CORE__ScalarDispatcher*)yap6_value_alloc(sizeof(YAP6__CORE__ScalarDispatcher));
  yap6_const_scalar_dispatcher->dispatcher = yap6_const_ident_dispatcher;
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_ident_dispatcher);

  yap6_const_scalar_dispatcher->APPLY = &scalar_dispatcher_APPLY;
  yap6_const_scalar_dispatcher->DESTR = &scalar_dispatcher_DESTR;
  yap6_const_scalar_dispatcher->FETCH = &scalar_dispatcher_FETCH;
  yap6_const_scalar_dispatcher->STORE = &scalar_dispatcher_STORE;

}

YAP6__CORE__Scalar* yap6_scalar_create(YAP6__CORE__Value* initialValue) {
  YAP6__CORE__Scalar* foo = (YAP6__CORE__Scalar*)yap6_value_alloc(sizeof(YAP6__CORE__Scalar));
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_scalar_dispatcher);
  foo->dispatcher = yap6_const_scalar_dispatcher;
  if (initialValue) {
    foo->cell = initialValue;
    yap6_value_refcnt_inc(initialValue);
  } else {
    foo->cell = yap6_const_undef;
    yap6_value_refcnt_inc(yap6_const_undef);
  }
  return foo;
}

void yap6_scalar_dispatcher_destr() {
  yap6_value_refcnt_dec((YAP6__CORE__Value*)yap6_const_scalar_dispatcher);
}
