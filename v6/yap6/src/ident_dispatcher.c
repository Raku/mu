#include "yap6.h"
#include <assert.h>
#include <stdlib.h>

static YAP6__CORE__Value* ident_dispatcher_APPLY(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Capture* arguments,
                                          YAP6__CORE__Value* wants) {
  return value;
}

static YAP6__CORE__Value* ident_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Capture* arguments,
                                          YAP6__CORE__Value* wants) {
  return NULL;
}

static int ident_dispatcher_COMPR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Capture* arguments) {
  // TODO get the argument from the capture...
  YAP6__CORE__Value* compare_to = (YAP6__CORE__Value*)compare_to;
  if ((int)value < (int)YAP6_APPLY(compare_to,NULL,value))
    return -1;
  else if ((int)value == (int)YAP6_APPLY(compare_to,NULL,value))
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
