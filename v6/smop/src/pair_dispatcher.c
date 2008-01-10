#include "vroom.h"
#include <assert.h>
#include <stdlib.h>


static void pair_dispatcher_DESTR(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* v) {
  VROOM__CORE__Pair* value = (VROOM__CORE__Pair*)v;
  VROOM__CORE__Value* key = value->key; value->key = NULL;
  VROOM__CORE__Value* val = value->value; value->value = NULL;
  vroom_value_refcnt_dec(key);
  vroom_value_refcnt_dec(val);
}

static VROOM__CORE__Value* pair_dispatcher_GTKEY(VROOM__CORE__Dispatcher* self,
                                  VROOM__CORE__Value* v) {
  VROOM__CORE__Pair* value = (VROOM__CORE__Pair*)v;
  vroom_value_rdlock(v);
  VROOM__CORE__Value* key = value->key;
  vroom_value_refcnt_inc(key);
  vroom_value_unlock(v);
  return key;
}

static VROOM__CORE__Value* pair_dispatcher_GTVAL(VROOM__CORE__Dispatcher* self,
                                  VROOM__CORE__Value* v) {
  VROOM__CORE__Pair* value = (VROOM__CORE__Pair*)v;
  vroom_value_rdlock(v);
  VROOM__CORE__Value* pairvalue = value->value;
  vroom_value_refcnt_inc(pairvalue);
  vroom_value_unlock(v);
  return pairvalue;
}

static VROOM__CORE__Value* pair_dispatcher_STVAL(VROOM__CORE__Dispatcher* self,
                                  VROOM__CORE__Value* v, VROOM__CORE__Value* newv) {
  VROOM__CORE__Pair* value = (VROOM__CORE__Pair*)v;
  vroom_value_wrlock(v);
  VROOM__CORE__Value* oldvalue = value->value;
  value->value = newv;
  vroom_value_refcnt_inc(newv);
  vroom_value_unlock(v);
  return oldvalue;
}

static VROOM__CORE__bytes* pair_dispatcher_WHICH(VROOM__CORE__Dispatcher* self,
                                   VROOM__CORE__Value* value) {
  vroom_value_rdlock(value);
  VROOM__CORE__Value* key = ((VROOM__CORE__Pair*)value)->key;
  vroom_value_unlock(value);
  return VROOM_WHICH(key);
}


VROOM__CORE__PairDispatcher* vroom_const_pair_dispatcher;

void vroom_pair_dispatcher_init() {
  vroom_const_pair_dispatcher = (VROOM__CORE__PairDispatcher*)vroom_value_alloc(sizeof(VROOM__CORE__PairDispatcher));
  vroom_const_pair_dispatcher->dispatcher = vroom_const_ident_dispatcher;
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_ident_dispatcher);
  vroom_const_pair_dispatcher->DESTR = &pair_dispatcher_DESTR;
  vroom_const_pair_dispatcher->GTKEY = &pair_dispatcher_GTKEY;
  vroom_const_pair_dispatcher->GTVAL = &pair_dispatcher_GTVAL;
  vroom_const_pair_dispatcher->STVAL = &pair_dispatcher_STVAL;
  vroom_const_pair_dispatcher->WHICH = &pair_dispatcher_WHICH;
}

VROOM__CORE__Pair* vroom_pair_create(VROOM__CORE__Value* key, VROOM__CORE__Value* value) {
  VROOM__CORE__Pair* val = (VROOM__CORE__Pair*)vroom_value_alloc(sizeof(VROOM__CORE__Pair));
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_pair_dispatcher);
  vroom_value_refcnt_inc(key);
  vroom_value_refcnt_inc(value);
  val->dispatcher = vroom_const_pair_dispatcher;
  val->key = key;
  val->value = value;
  return val;
}

void vroom_pair_dispatcher_destr() {
  vroom_value_refcnt_dec((VROOM__CORE__Value*)vroom_const_pair_dispatcher);
}
