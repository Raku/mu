#include "yap6.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static YAP6__CORE__Value* ident_dispatcher_APPLY(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__List* arguments,
                                          YAP6__CORE__Value* wants) {
  return value;
}

static void ident_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
}

static YAP6__CORE__bytes* ident_dispatcher_WHICH(YAP6__CORE__Dispatcher* self,
                                   YAP6__CORE__Value* value) {
  char str[32];
  sprintf(str, "value:%p", value);
  int len = strlen(str);
  return yap6_bytes_create(str, len);

}

YAP6__CORE__Dispatcher* yap6_const_ident_dispatcher;

void yap6_ident_dispatcher_init() {
  yap6_const_ident_dispatcher = (YAP6__CORE__Dispatcher*)yap6_value_alloc(sizeof(YAP6__CORE__Dispatcher));
  yap6_const_ident_dispatcher->APPLY = &ident_dispatcher_APPLY;
  yap6_const_ident_dispatcher->DESTR = &ident_dispatcher_DESTR;
}

void yap6_ident_dispatcher_destr() {
  yap6_value_refcnt_dec((YAP6__CORE__Value*)yap6_const_ident_dispatcher);
}

void yap6_ident_dispatcher_which_init() {
  yap6_const_ident_dispatcher->WHICH = &ident_dispatcher_WHICH;
}
