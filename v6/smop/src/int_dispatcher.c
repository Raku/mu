#include "vroom.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


static void int_dispatcher_DESTR(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value) {
  // does nothing
}

static VROOM__CORE__bytes* int_dispatcher_WHICH(VROOM__CORE__Dispatcher* self,
                                               VROOM__CORE__Value* value) {
  char res[16];
  vroom_value_rdlock(value);
  sprintf(res, "%d", ((VROOM__CORE__int*)value)->value);
  vroom_value_unlock(value);
  int size = strlen(res);
  return vroom_bytes_create(res, size);
  
}

static VROOM__CORE__Value* int_dispatcher_BOOLN(VROOM__CORE__Dispatcher* self,
                                               VROOM__CORE__Value* value) {
  vroom_value_rdlock(value);
  int v = ((VROOM__CORE__int*)value)->value;
  vroom_value_unlock(value);
  if (v) {
    return vroom_value_refcnt_inc(value);
  } else {
    return vroom_value_refcnt_inc(vroom_bool_false);
  }
}

VROOM__CORE__Dispatcher* vroom_const_int_dispatcher;

void vroom_int_dispatcher_init() {
  vroom_const_int_dispatcher = (VROOM__CORE__Dispatcher*)vroom_value_alloc(sizeof(VROOM__CORE__Dispatcher));
  vroom_const_int_dispatcher->DESTR = &int_dispatcher_DESTR;
  vroom_const_int_dispatcher->BOOLN = &int_dispatcher_BOOLN;
}

VROOM__CORE__int* vroom_int_create(int lowl) {
  VROOM__CORE__int* foo = (VROOM__CORE__int*)vroom_value_alloc(sizeof(VROOM__CORE__int));
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_int_dispatcher);
  foo->dispatcher = vroom_const_int_dispatcher;
  foo->value = lowl;
  return foo;
}

int vroom_int_lowlevel(VROOM__CORE__int* val) {
  vroom_value_rdlock((VROOM__CORE__Value*)val);
  int v = val->value;
  vroom_value_unlock((VROOM__CORE__Value*)val);
  return v;
}

void vroom_int_dispatcher_destr() {
  vroom_value_refcnt_dec((VROOM__CORE__Value*)vroom_const_int_dispatcher);
}

void vroom_int_dispatcher_which_init() {
  vroom_const_int_dispatcher->WHICH = &int_dispatcher_WHICH;
}
