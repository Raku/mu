#include "smop.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>


static void scalar_dispatcher_DESTR(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value) {
  smop_value_wrlock(value);
  SMOP__CORE__Value* cell = ((SMOP__CORE__Scalar*)value)->cell;
  ((SMOP__CORE__Scalar*)value)->cell = NULL;
  smop_value_unlock(value);
  if (cell) {
    smop_value_refcnt_dec(cell);
  }
}


static SMOP__CORE__Value* scalar_dispatcher_FETCH(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value,
                                          SMOP__CORE__Value* wants) {

  smop_value_wrlock(value);
  SMOP__CORE__Value* val = ((SMOP__CORE__Scalar*)value)->cell;
  smop_value_unlock(value);
  smop_value_refcnt_inc(val);
  return val;
}

static SMOP__CORE__Value* scalar_dispatcher_STORE(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value,
                                          SMOP__CORE__Value* newvalue) {
  smop_value_wrlock(value);
  SMOP__CORE__Value* oldval = ((SMOP__CORE__Scalar*)value)->cell;
  ((SMOP__CORE__Scalar*)value)->cell = newvalue;
  smop_value_unlock(value);
  smop_value_refcnt_inc(newvalue);
  return oldval;
}

static SMOP__CORE__bytes* scalar_dispatcher_WHICH(SMOP__CORE__Dispatcher* self,
                                   SMOP__CORE__Value* value) {
  smop_value_wrlock(value);
  SMOP__CORE__Value* val = ((SMOP__CORE__Scalar*)value)->cell;
  smop_value_unlock(value);
  return SMOP_WHICH(val);

}


SMOP__CORE__ScalarDispatcher* smop_const_scalar_dispatcher;

void smop_scalar_dispatcher_init() {

  smop_const_scalar_dispatcher = (SMOP__CORE__ScalarDispatcher*)smop_value_alloc(sizeof(SMOP__CORE__ScalarDispatcher));
  smop_const_scalar_dispatcher->dispatcher = smop_const_ident_dispatcher;
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_ident_dispatcher);

  smop_const_scalar_dispatcher->DESTR = &scalar_dispatcher_DESTR;
  smop_const_scalar_dispatcher->FETCH = &scalar_dispatcher_FETCH;
  smop_const_scalar_dispatcher->STORE = &scalar_dispatcher_STORE;
  smop_const_scalar_dispatcher->WHICH = &scalar_dispatcher_WHICH;

}

SMOP__CORE__Scalar* smop_scalar_create(SMOP__CORE__Value* initialValue) {
  SMOP__CORE__Scalar* foo = (SMOP__CORE__Scalar*)smop_value_alloc(sizeof(SMOP__CORE__Scalar));
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_scalar_dispatcher);
  foo->dispatcher = smop_const_scalar_dispatcher;
  if (initialValue) {
    foo->cell = initialValue;
    smop_value_refcnt_inc(initialValue);
  } else {
    foo->cell = smop_const_undef;
    smop_value_refcnt_inc(smop_const_undef);
  }
  return foo;
}

void smop_scalar_dispatcher_destr() {
  smop_value_refcnt_dec((SMOP__CORE__Value*)smop_const_scalar_dispatcher);
}
