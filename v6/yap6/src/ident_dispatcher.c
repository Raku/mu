#include "yap6.h"
#include <assert.h>
#include <stdlib.h>

static YAP6__CORE__Value* ident_dispatcher_APPLY(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Capture* arguments,
                                          YAP6__CORE__Value* wants) {
  return value;
}

static void ident_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
}

static int ident_dispatcher_COMPR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* other) {
  if ((int)value == (int)other)
    return 0;
  else
    return 1;
}

YAP6__CORE__Dispatcher* yap6_const_ident_dispatcher;

void yap6_ident_dispatcher_init() {
  yap6_const_ident_dispatcher = (YAP6__CORE__Dispatcher*)yap6_value_alloc(sizeof(YAP6__CORE__Dispatcher));
  yap6_const_ident_dispatcher->APPLY = &ident_dispatcher_APPLY;
  yap6_const_ident_dispatcher->DESTR = &ident_dispatcher_DESTR;
  yap6_const_ident_dispatcher->COMPR = &ident_dispatcher_COMPR;
}

void yap6_ident_dispatcher_destr() {
  yap6_value_refcnt_dec((YAP6__CORE__Value*)yap6_const_ident_dispatcher);
}
