#include "vroom.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static void bytes_dispatcher_DESTR(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value) {
  vroom_value_wrlock(value);
  VROOM__CORE__bytes* v = (VROOM__CORE__bytes*)value;
  free(v->value);
  v->value = NULL;
  vroom_value_unlock(value);
}

static VROOM__CORE__Value* bytes_dispatcher_BOOLN(VROOM__CORE__Dispatcher* self,
                                                 VROOM__CORE__Value* value) {
  vroom_value_rdlock(value);
  int l = ((VROOM__CORE__bytes*)value)->size;
  vroom_value_unlock(value);
  if (l) {
    return vroom_value_refcnt_inc(value);
  } else {
    return vroom_value_refcnt_inc(vroom_bool_false);
  }
}

static VROOM__CORE__bytes* bytes_dispatcher_WHICH(VROOM__CORE__Dispatcher* self,
                                               VROOM__CORE__Value* value) {
  return (VROOM__CORE__bytes*)vroom_value_refcnt_inc(value);
}

VROOM__CORE__Dispatcher* vroom_const_bytes_dispatcher;

void vroom_bytes_dispatcher_init() {
  vroom_const_bytes_dispatcher = (VROOM__CORE__Dispatcher*)vroom_value_alloc(sizeof(VROOM__CORE__Dispatcher));
  vroom_const_bytes_dispatcher->dispatcher = vroom_const_ident_dispatcher;
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_ident_dispatcher);
  vroom_const_bytes_dispatcher->DESTR = &bytes_dispatcher_DESTR;
  vroom_const_bytes_dispatcher->WHICH = &bytes_dispatcher_WHICH;
  vroom_const_bytes_dispatcher->BOOLN = &bytes_dispatcher_BOOLN;
}

VROOM__CORE__bytes* vroom_bytes_create(const char* lowl, int size) {
  VROOM__CORE__bytes* foo = (VROOM__CORE__bytes*)vroom_value_alloc(sizeof(VROOM__CORE__bytes));
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_bytes_dispatcher);
  foo->dispatcher = vroom_const_bytes_dispatcher;
  foo->value = malloc(size);
  memcpy(foo->value, lowl, size);
  foo->size = size;
  return foo;
}

char* vroom_bytes_lowlevel(VROOM__CORE__bytes* val, int* size_ret) {
  vroom_value_rdlock((VROOM__CORE__Value*)val);
  char* v = val->value;
  *size_ret = val->size;
  vroom_value_unlock((VROOM__CORE__Value*)val);
  return v;
}

void vroom_bytes_dispatcher_destr() {
  vroom_value_refcnt_dec((VROOM__CORE__Value*)vroom_const_bytes_dispatcher);
}
