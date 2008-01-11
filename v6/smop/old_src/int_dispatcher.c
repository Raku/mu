#include "smop.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


static void int_dispatcher_DESTR(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value) {
  // does nothing
}

static SMOP__CORE__bytes* int_dispatcher_WHICH(SMOP__CORE__Dispatcher* self,
                                               SMOP__CORE__Value* value) {
  char res[16];
  smop_value_rdlock(value);
  sprintf(res, "%d", ((SMOP__CORE__int*)value)->value);
  smop_value_unlock(value);
  int size = strlen(res);
  return smop_bytes_create(res, size);
  
}

static SMOP__CORE__Value* int_dispatcher_BOOLN(SMOP__CORE__Dispatcher* self,
                                               SMOP__CORE__Value* value) {
  smop_value_rdlock(value);
  int v = ((SMOP__CORE__int*)value)->value;
  smop_value_unlock(value);
  if (v) {
    return smop_value_refcnt_inc(value);
  } else {
    return smop_value_refcnt_inc(smop_bool_false);
  }
}

SMOP__CORE__Dispatcher* smop_const_int_dispatcher;

void smop_int_dispatcher_init() {
  smop_const_int_dispatcher = (SMOP__CORE__Dispatcher*)smop_value_alloc(sizeof(SMOP__CORE__Dispatcher));
  smop_const_int_dispatcher->DESTR = &int_dispatcher_DESTR;
  smop_const_int_dispatcher->BOOLN = &int_dispatcher_BOOLN;
}

SMOP__CORE__int* smop_int_create(int lowl) {
  SMOP__CORE__int* foo = (SMOP__CORE__int*)smop_value_alloc(sizeof(SMOP__CORE__int));
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_int_dispatcher);
  foo->dispatcher = smop_const_int_dispatcher;
  foo->value = lowl;
  return foo;
}

int smop_int_lowlevel(SMOP__CORE__int* val) {
  smop_value_rdlock((SMOP__CORE__Value*)val);
  int v = val->value;
  smop_value_unlock((SMOP__CORE__Value*)val);
  return v;
}

void smop_int_dispatcher_destr() {
  smop_value_refcnt_dec((SMOP__CORE__Value*)smop_const_int_dispatcher);
}

void smop_int_dispatcher_which_init() {
  smop_const_int_dispatcher->WHICH = &int_dispatcher_WHICH;
}
