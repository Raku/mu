#include "smop.h"
#include <assert.h>
#include <stdlib.h>


static void pair_dispatcher_DESTR(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* v) {
  SMOP__CORE__Pair* value = (SMOP__CORE__Pair*)v;
  SMOP__CORE__Value* key = value->key; value->key = NULL;
  SMOP__CORE__Value* val = value->value; value->value = NULL;
  smop_value_refcnt_dec(key);
  smop_value_refcnt_dec(val);
}

static SMOP__CORE__Value* pair_dispatcher_GTKEY(SMOP__CORE__Dispatcher* self,
                                  SMOP__CORE__Value* v) {
  SMOP__CORE__Pair* value = (SMOP__CORE__Pair*)v;
  smop_value_rdlock(v);
  SMOP__CORE__Value* key = value->key;
  smop_value_refcnt_inc(key);
  smop_value_unlock(v);
  return key;
}

static SMOP__CORE__Value* pair_dispatcher_GTVAL(SMOP__CORE__Dispatcher* self,
                                  SMOP__CORE__Value* v) {
  SMOP__CORE__Pair* value = (SMOP__CORE__Pair*)v;
  smop_value_rdlock(v);
  SMOP__CORE__Value* pairvalue = value->value;
  smop_value_refcnt_inc(pairvalue);
  smop_value_unlock(v);
  return pairvalue;
}

static SMOP__CORE__Value* pair_dispatcher_STVAL(SMOP__CORE__Dispatcher* self,
                                  SMOP__CORE__Value* v, SMOP__CORE__Value* newv) {
  SMOP__CORE__Pair* value = (SMOP__CORE__Pair*)v;
  smop_value_wrlock(v);
  SMOP__CORE__Value* oldvalue = value->value;
  value->value = newv;
  smop_value_refcnt_inc(newv);
  smop_value_unlock(v);
  return oldvalue;
}

static SMOP__CORE__bytes* pair_dispatcher_WHICH(SMOP__CORE__Dispatcher* self,
                                   SMOP__CORE__Value* value) {
  smop_value_rdlock(value);
  SMOP__CORE__Value* key = ((SMOP__CORE__Pair*)value)->key;
  smop_value_unlock(value);
  return SMOP_WHICH(key);
}


SMOP__CORE__PairDispatcher* smop_const_pair_dispatcher;

void smop_pair_dispatcher_init() {
  smop_const_pair_dispatcher = (SMOP__CORE__PairDispatcher*)smop_value_alloc(sizeof(SMOP__CORE__PairDispatcher));
  smop_const_pair_dispatcher->dispatcher = smop_const_ident_dispatcher;
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_ident_dispatcher);
  smop_const_pair_dispatcher->DESTR = &pair_dispatcher_DESTR;
  smop_const_pair_dispatcher->GTKEY = &pair_dispatcher_GTKEY;
  smop_const_pair_dispatcher->GTVAL = &pair_dispatcher_GTVAL;
  smop_const_pair_dispatcher->STVAL = &pair_dispatcher_STVAL;
  smop_const_pair_dispatcher->WHICH = &pair_dispatcher_WHICH;
}

SMOP__CORE__Pair* smop_pair_create(SMOP__CORE__Value* key, SMOP__CORE__Value* value) {
  SMOP__CORE__Pair* val = (SMOP__CORE__Pair*)smop_value_alloc(sizeof(SMOP__CORE__Pair));
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_pair_dispatcher);
  smop_value_refcnt_inc(key);
  smop_value_refcnt_inc(value);
  val->dispatcher = smop_const_pair_dispatcher;
  val->key = key;
  val->value = value;
  return val;
}

void smop_pair_dispatcher_destr() {
  smop_value_refcnt_dec((SMOP__CORE__Value*)smop_const_pair_dispatcher);
}
