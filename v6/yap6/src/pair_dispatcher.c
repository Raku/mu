#include "yap6.h"
#include <assert.h>
#include <stdlib.h>

static YAP6__CORE__Value* pair_dispatcher_APPLY(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__List* arguments,
                                          YAP6__CORE__Value* wants) {
  // TODO
  return value;
}

static void pair_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* v) {
  YAP6__CORE__Pair* value = (YAP6__CORE__Pair*)v;
  YAP6__CORE__Value* key = value->key; value->key = NULL;
  YAP6__CORE__Value* val = value->value; value->value = NULL;
  yap6_value_refcnt_dec(key);
  yap6_value_refcnt_dec(val);
}

static YAP6__CORE__Value* pair_dispatcher_GTKEY(YAP6__CORE__Dispatcher* self,
                                  YAP6__CORE__Value* v) {
  YAP6__CORE__Pair* value = (YAP6__CORE__Pair*)v;
  yap6_value_rdlock(v);
  YAP6__CORE__Value* key = value->key;
  yap6_value_refcnt_inc(key);
  yap6_value_unlock(v);
  return key;
}

static YAP6__CORE__Value* pair_dispatcher_GTVAL(YAP6__CORE__Dispatcher* self,
                                  YAP6__CORE__Value* v) {
  YAP6__CORE__Pair* value = (YAP6__CORE__Pair*)v;
  yap6_value_rdlock(v);
  YAP6__CORE__Value* pairvalue = value->value;
  yap6_value_refcnt_inc(pairvalue);
  yap6_value_unlock(v);
  return pairvalue;
}

static YAP6__CORE__Value* pair_dispatcher_STVAL(YAP6__CORE__Dispatcher* self,
                                  YAP6__CORE__Value* v, YAP6__CORE__Value* newv) {
  YAP6__CORE__Pair* value = (YAP6__CORE__Pair*)v;
  yap6_value_wrlock(v);
  YAP6__CORE__Value* oldvalue = value->value;
  value->value = newv;
  yap6_value_refcnt_inc(newv);
  yap6_value_unlock(v);
  return oldvalue;
}

static YAP6__CORE__bytes* pair_dispatcher_WHICH(YAP6__CORE__Dispatcher* self,
                                   YAP6__CORE__Value* value) {
  yap6_value_rdlock(value);
  YAP6__CORE__Value* key = ((YAP6__CORE__Pair*)value)->key;
  yap6_value_unlock(value);
  return YAP6_WHICH(key);
}


YAP6__CORE__PairDispatcher* yap6_const_pair_dispatcher;

void yap6_pair_dispatcher_init() {
  yap6_const_pair_dispatcher = (YAP6__CORE__PairDispatcher*)yap6_value_alloc(sizeof(YAP6__CORE__PairDispatcher));
  yap6_const_pair_dispatcher->dispatcher = yap6_const_ident_dispatcher;
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_ident_dispatcher);
  yap6_const_pair_dispatcher->APPLY = &pair_dispatcher_APPLY;
  yap6_const_pair_dispatcher->DESTR = &pair_dispatcher_DESTR;
  yap6_const_pair_dispatcher->GTKEY = &pair_dispatcher_GTKEY;
  yap6_const_pair_dispatcher->GTVAL = &pair_dispatcher_GTVAL;
  yap6_const_pair_dispatcher->STVAL = &pair_dispatcher_STVAL;
}

YAP6__CORE__Pair* yap6_pair_create(YAP6__CORE__Value* key, YAP6__CORE__Value* value) {
  YAP6__CORE__Pair* val = (YAP6__CORE__Pair*)yap6_value_alloc(sizeof(YAP6__CORE__Pair));
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_pair_dispatcher);
  yap6_value_refcnt_inc(key);
  yap6_value_refcnt_inc(value);
  val->dispatcher = yap6_const_pair_dispatcher;
  val->key = key;
  val->value = value;
  return val;
}

void yap6_pair_dispatcher_destr() {
  yap6_value_refcnt_dec((YAP6__CORE__Value*)yap6_const_pair_dispatcher);
}
