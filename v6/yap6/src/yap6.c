#include "yap6.h"
#include <stdlib.h>
#include <assert.h>

YAP6__CORE__Value* ident_dispatcher_APPLY(YAP6__CORE__Value* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* arguments,
                                          YAP6__CORE__Value* wants) {
  return value;
}

YAP6__CORE__Value* ident_dispatcher_DESTR(YAP6__CORE__Value* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* arguments,
                                          YAP6__CORE__Value* wants) {
  free(value);
  return NULL;
}

int ident_dispatcher_COMPR(YAP6__CORE__Value* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* arguments) {
  if ((int)value < (int)arguments)
    return -1;
  else if ((int)value == (int)arguments)
    return 0;
  else
    return 1;

}

YAP6__CORE__Value* yap6_const_undef;
YAP6__CORE__Value* yap6_const_true;
YAP6__CORE__Value* yap6_const_false;

int main(int arc, char** argv) {

  YAP6__CORE__Dispatcher* ident_dispatcher = calloc(1,sizeof(YAP6__CORE__Dispatcher));
  assert(ident_dispatcher);
  ident_dispatcher->APPLY = &ident_dispatcher_APPLY;
  ident_dispatcher->DESTR = &ident_dispatcher_DESTR;
  ident_dispatcher->COMPR = &ident_dispatcher_COMPR;

  yap6_const_undef = calloc(1,sizeof(YAP6__CORE__Value));
  assert(yap6_const_undef);
  yap6_const_undef->dispatcher = ident_dispatcher;

  yap6_const_true = calloc(1,sizeof(YAP6__CORE__Value));
  assert(yap6_const_true);
  yap6_const_true->dispatcher = ident_dispatcher;

  yap6_const_false = calloc(1,sizeof(YAP6__CORE__Value));
  assert(yap6_const_false);
  yap6_const_false->dispatcher = ident_dispatcher;

  YAP6__CORE__HashDispatcher* prototype_class_class_dispatcher = calloc(1,sizeof(YAP6__CORE__HashDispatcher));
  assert(prototype_class_class_dispatcher);
  
  YAP6__CORE__Hash* prototype_class_class = calloc(1,sizeof(YAP6__CORE__Hash));
  assert(prototype_class_class);
  
  prototype_class_class->dispatcher = prototype_class_class_dispatcher;

  return 0;
}
