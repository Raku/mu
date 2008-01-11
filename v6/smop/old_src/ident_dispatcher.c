#include "smop.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


static void ident_dispatcher_DESTR(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value) {
}

static SMOP__CORE__bytes* ident_dispatcher_WHICH(SMOP__CORE__Dispatcher* self,
                                   SMOP__CORE__Value* value) {
  char str[32];
  sprintf(str, "value:%p", value);
  int len = strlen(str);
  return smop_bytes_create(str, len);

}

SMOP__CORE__Dispatcher* smop_const_ident_dispatcher;

void smop_ident_dispatcher_init() {
  smop_const_ident_dispatcher = (SMOP__CORE__Dispatcher*)smop_value_alloc(sizeof(SMOP__CORE__Dispatcher));
  smop_const_ident_dispatcher->DESTR = &ident_dispatcher_DESTR;
}

void smop_ident_dispatcher_destr() {
  smop_value_refcnt_dec((SMOP__CORE__Value*)smop_const_ident_dispatcher);
}

void smop_ident_dispatcher_which_init() {
  smop_const_ident_dispatcher->WHICH = &ident_dispatcher_WHICH;
}
