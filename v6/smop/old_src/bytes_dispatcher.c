#include "smop.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static void bytes_dispatcher_DESTR(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value) {
  smop_value_wrlock(value);
  SMOP__CORE__bytes* v = (SMOP__CORE__bytes*)value;
  free(v->value);
  v->value = NULL;
  smop_value_unlock(value);
}

static SMOP__CORE__Value* bytes_dispatcher_BOOLN(SMOP__CORE__Dispatcher* self,
                                                 SMOP__CORE__Value* value) {
  smop_value_rdlock(value);
  int l = ((SMOP__CORE__bytes*)value)->size;
  smop_value_unlock(value);
  if (l) {
    return smop_value_refcnt_inc(value);
  } else {
    return smop_value_refcnt_inc(smop_bool_false);
  }
}

static SMOP__CORE__bytes* bytes_dispatcher_WHICH(SMOP__CORE__Dispatcher* self,
                                               SMOP__CORE__Value* value) {
  return (SMOP__CORE__bytes*)smop_value_refcnt_inc(value);
}

SMOP__CORE__Dispatcher* smop_const_bytes_dispatcher;

void smop_bytes_dispatcher_init() {
  smop_const_bytes_dispatcher = (SMOP__CORE__Dispatcher*)smop_value_alloc(sizeof(SMOP__CORE__Dispatcher));
  smop_const_bytes_dispatcher->dispatcher = smop_const_ident_dispatcher;
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_ident_dispatcher);
  smop_const_bytes_dispatcher->DESTR = &bytes_dispatcher_DESTR;
  smop_const_bytes_dispatcher->WHICH = &bytes_dispatcher_WHICH;
  smop_const_bytes_dispatcher->BOOLN = &bytes_dispatcher_BOOLN;
}

SMOP__CORE__bytes* smop_bytes_create(const char* lowl, int size) {
  SMOP__CORE__bytes* foo = (SMOP__CORE__bytes*)smop_value_alloc(sizeof(SMOP__CORE__bytes));
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_bytes_dispatcher);
  foo->dispatcher = smop_const_bytes_dispatcher;
  foo->value = malloc(size);
  memcpy(foo->value, lowl, size);
  foo->size = size;
  return foo;
}

char* smop_bytes_lowlevel(SMOP__CORE__bytes* val, int* size_ret) {
  smop_value_rdlock((SMOP__CORE__Value*)val);
  char* v = val->value;
  *size_ret = val->size;
  smop_value_unlock((SMOP__CORE__Value*)val);
  return v;
}

void smop_bytes_dispatcher_destr() {
  smop_value_refcnt_dec((SMOP__CORE__Value*)smop_const_bytes_dispatcher);
}
