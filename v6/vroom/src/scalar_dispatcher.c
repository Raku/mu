#include "vroom.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>


static void scalar_dispatcher_DESTR(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value) {
  vroom_value_wrlock(value);
  VROOM__CORE__Value* cell = ((VROOM__CORE__Scalar*)value)->cell;
  ((VROOM__CORE__Scalar*)value)->cell = NULL;
  vroom_value_unlock(value);
  if (cell) {
    vroom_value_refcnt_dec(cell);
  }
}


static VROOM__CORE__Value* scalar_dispatcher_FETCH(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value,
                                          VROOM__CORE__Value* wants) {

  vroom_value_wrlock(value);
  VROOM__CORE__Value* val = ((VROOM__CORE__Scalar*)value)->cell;
  vroom_value_unlock(value);
  vroom_value_refcnt_inc(val);
  return val;
}

static VROOM__CORE__Value* scalar_dispatcher_STORE(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value,
                                          VROOM__CORE__Value* newvalue) {
  vroom_value_wrlock(value);
  VROOM__CORE__Value* oldval = ((VROOM__CORE__Scalar*)value)->cell;
  ((VROOM__CORE__Scalar*)value)->cell = newvalue;
  vroom_value_unlock(value);
  vroom_value_refcnt_inc(newvalue);
  return oldval;
}

static VROOM__CORE__bytes* scalar_dispatcher_WHICH(VROOM__CORE__Dispatcher* self,
                                   VROOM__CORE__Value* value) {
  vroom_value_wrlock(value);
  VROOM__CORE__Value* val = ((VROOM__CORE__Scalar*)value)->cell;
  vroom_value_unlock(value);
  return VROOM_WHICH(val);

}


VROOM__CORE__ScalarDispatcher* vroom_const_scalar_dispatcher;

void vroom_scalar_dispatcher_init() {

  vroom_const_scalar_dispatcher = (VROOM__CORE__ScalarDispatcher*)vroom_value_alloc(sizeof(VROOM__CORE__ScalarDispatcher));
  vroom_const_scalar_dispatcher->dispatcher = vroom_const_ident_dispatcher;
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_ident_dispatcher);

  vroom_const_scalar_dispatcher->DESTR = &scalar_dispatcher_DESTR;
  vroom_const_scalar_dispatcher->FETCH = &scalar_dispatcher_FETCH;
  vroom_const_scalar_dispatcher->STORE = &scalar_dispatcher_STORE;
  vroom_const_scalar_dispatcher->WHICH = &scalar_dispatcher_WHICH;

}

VROOM__CORE__Scalar* vroom_scalar_create(VROOM__CORE__Value* initialValue) {
  VROOM__CORE__Scalar* foo = (VROOM__CORE__Scalar*)vroom_value_alloc(sizeof(VROOM__CORE__Scalar));
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_scalar_dispatcher);
  foo->dispatcher = vroom_const_scalar_dispatcher;
  if (initialValue) {
    foo->cell = initialValue;
    vroom_value_refcnt_inc(initialValue);
  } else {
    foo->cell = vroom_const_undef;
    vroom_value_refcnt_inc(vroom_const_undef);
  }
  return foo;
}

void vroom_scalar_dispatcher_destr() {
  vroom_value_refcnt_dec((VROOM__CORE__Value*)vroom_const_scalar_dispatcher);
}
