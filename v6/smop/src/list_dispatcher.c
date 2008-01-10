#include "smop.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct SMOP__CORE__List__ProxyScalar {
  SMOP__BASE__Value
  SMOP__CORE__ScalarDispatcher* dispatcher;
  SMOP__CORE__Value* cell;
  SMOP__CORE__List* owner;
  SMOP__CORE__int* index;
} SMOP__CORE__List__ProxyScalar;

SMOP__CORE__ScalarDispatcher* smop_const_list_proxyscalar_dispatcher;

SMOP__CORE__List__ProxyScalar* smop_list_proxyscalar_create() {
  SMOP__CORE__List__ProxyScalar* foo = (SMOP__CORE__List__ProxyScalar*)smop_value_alloc(sizeof(SMOP__CORE__List__ProxyScalar));
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_list_proxyscalar_dispatcher);
  foo->dispatcher = (SMOP__CORE__ScalarDispatcher*)smop_const_list_proxyscalar_dispatcher;
  return foo;
}




static void list_dispatcher_DESTR(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value) {
  smop_value_wrlock(value);
  SMOP__CORE__List* list = (SMOP__CORE__List*)value;
  int length = list->length;
  SMOP__CORE__Value** items = list->items;
  list->length = 0;
  list->items = NULL;
  smop_value_unlock(value);
  int i;
  for (i = 0; i < length; i++) {
    if (items[i]) {
      smop_value_refcnt_dec(items[i]);
      items[i] = NULL;
    }
  }
  free(items);
}

static SMOP__CORE__Scalar* list_dispatcher_LOOKP(SMOP__CORE__Dispatcher* self,
                                                 SMOP__CORE__Value* v_value,
                                                 SMOP__CORE__int* index) {
  SMOP__CORE__List* value = (SMOP__CORE__List*)v_value;
  int wanted = smop_int_lowlevel(index);
  smop_value_wrlock(v_value);
  SMOP__CORE__Scalar* ret = NULL;
  if (value->length > wanted && value->items[wanted]) {
    ret = (SMOP__CORE__Scalar*)value->items[wanted];
    smop_value_refcnt_inc((SMOP__CORE__Value*)ret);
    smop_value_unlock(v_value);
  } else {
    smop_value_unlock(v_value);
    SMOP__CORE__List__ProxyScalar* p = smop_list_proxyscalar_create();
    // no need to lock here...
    p->cell = smop_const_undef;
    smop_value_refcnt_inc(smop_const_undef);
    p->index = index;
    smop_value_refcnt_inc((SMOP__CORE__Value*)index);
    p->owner = value;
    smop_value_refcnt_inc(v_value);
    ret = (SMOP__CORE__Scalar*)p;
  }
  return ret;
}

static SMOP__CORE__Scalar* list_dispatcher_EXIST(SMOP__CORE__Dispatcher* self,
                                                 SMOP__CORE__Value* v_value,
                                                 SMOP__CORE__int* index) {
  int wanted = smop_int_lowlevel(index);
  smop_value_wrlock(v_value);
  SMOP__CORE__List* value = (SMOP__CORE__List*)v_value;
  SMOP__CORE__Scalar* ret = NULL;
  if (value->length > wanted) {
    SMOP__CORE__Scalar* ret = (SMOP__CORE__Scalar*)value->items[wanted];
    smop_value_refcnt_inc((SMOP__CORE__Value*)ret);
  }
  smop_value_unlock(v_value);
  return ret;
}

static SMOP__CORE__Scalar* list_dispatcher_DELET(SMOP__CORE__Dispatcher* self,
                                                 SMOP__CORE__Value* v_value,
                                                 SMOP__CORE__int* index) {
  int wanted = smop_int_lowlevel(index);
  smop_value_wrlock(v_value);
  SMOP__CORE__List* value = (SMOP__CORE__List*)v_value;
  SMOP__CORE__Scalar* ret = NULL;
  if (value->length >= wanted) {
    ret = (SMOP__CORE__Scalar*)value->items[wanted];
    value->items[wanted] = smop_const_undef;
    smop_value_refcnt_inc(smop_const_undef);
  }
  smop_value_unlock(v_value);
  return ret;
}

static SMOP__CORE__bytes* list_dispatcher_WHICH(SMOP__CORE__Dispatcher* self,
                                   SMOP__CORE__Value* value) {
  char str[32];
  sprintf(str, "list:%p", value);
  int len = strlen(str);
  return smop_bytes_create(str, len);

}

static SMOP__CORE__int* list_dispatcher_ELEMS(SMOP__CORE__Dispatcher* self,
                                   SMOP__CORE__Value* value) {
  smop_value_rdlock(value);
  int length = ((SMOP__CORE__List*)value)->length;
  smop_value_unlock(value);
  return smop_int_create(length);

}


static void list_proxyscalar_dispatcher_DESTR(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value) {
  smop_value_wrlock(value);
  SMOP__CORE__Value* cell = ((SMOP__CORE__Scalar*)value)->cell;
  SMOP__CORE__List* owner = ((SMOP__CORE__List__ProxyScalar*)value)->owner;
  ((SMOP__CORE__Scalar*)value)->cell = NULL;
  ((SMOP__CORE__List__ProxyScalar*)value)->owner = NULL;
  smop_value_unlock(value);
  if (cell) {
    smop_value_refcnt_dec(cell);
  }
  if (owner) {
    smop_value_refcnt_dec((SMOP__CORE__Value*)owner);
  }
}


static SMOP__CORE__Value* list_proxyscalar_dispatcher_FETCH(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value,
                                          SMOP__CORE__Value* wants) {

  smop_value_wrlock(value);
  SMOP__CORE__Value* val = ((SMOP__CORE__Scalar*)value)->cell;
  smop_value_unlock(value);
  smop_value_refcnt_inc(val);
  return val;
}

static SMOP__CORE__Value* list_proxyscalar_dispatcher_STORE(SMOP__CORE__Dispatcher* self,
                                          SMOP__CORE__Value* value,
                                          SMOP__CORE__Value* newvalue) {
  smop_value_wrlock(value);
  SMOP__CORE__Value* oldval = ((SMOP__CORE__Scalar*)value)->cell;
  ((SMOP__CORE__Scalar*)value)->cell = newvalue;
  SMOP__CORE__List* list = ((SMOP__CORE__List__ProxyScalar*)value)->owner;
  SMOP__CORE__int* index_i = ((SMOP__CORE__List__ProxyScalar*)value)->index;
  int index = smop_int_lowlevel(index_i);
  smop_value_unlock(value);
  if (list) {
    smop_value_wrlock((SMOP__CORE__Value*)list);
    if (list->length <= index) {
      list->items = (SMOP__CORE__Value**)realloc(list->items, sizeof(void*)*(index+1));
      assert(list->items);
      memset(list->items[list->length - 1], 0, list->length - index);
      list->length = index + 1;
    }
    if (list->items[index] == NULL) {
      list->items[index] = value;
    }
    smop_value_refcnt_inc(value);
    smop_value_unlock((SMOP__CORE__Value*)list);
    smop_value_wrlock(value);
    ((SMOP__CORE__List__ProxyScalar*)value)->owner = NULL;
    smop_value_refcnt_dec((SMOP__CORE__Value*)((SMOP__CORE__List__ProxyScalar*)value)->index);
    ((SMOP__CORE__List__ProxyScalar*)value)->index = NULL;
    smop_value_refcnt_dec((SMOP__CORE__Value*)list);
    smop_value_unlock(value);
  }
  smop_value_refcnt_inc(newvalue);
  return oldval;
}

static SMOP__CORE__bytes* list_proxyscalar_dispatcher_WHICH(SMOP__CORE__Dispatcher* self,
                                   SMOP__CORE__Value* value) {
  smop_value_wrlock(value);
  SMOP__CORE__Value* val = ((SMOP__CORE__Scalar*)value)->cell;
  smop_value_unlock(value);
  return SMOP_WHICH(val);
}


SMOP__CORE__ListDispatcher* smop_const_list_dispatcher;

void smop_list_dispatcher_init() {
  smop_const_list_dispatcher = (SMOP__CORE__ListDispatcher*)smop_value_alloc(sizeof(SMOP__CORE__ListDispatcher));
  smop_const_list_dispatcher->dispatcher = smop_const_ident_dispatcher;
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_ident_dispatcher);
  smop_const_list_dispatcher->DESTR = &list_dispatcher_DESTR;
  smop_const_list_dispatcher->LOOKP = &list_dispatcher_LOOKP;
  smop_const_list_dispatcher->EXIST = &list_dispatcher_EXIST;
  smop_const_list_dispatcher->DELET = &list_dispatcher_DELET;
  smop_const_list_dispatcher->WHICH = &list_dispatcher_WHICH;
  smop_const_list_dispatcher->ELEMS = &list_dispatcher_ELEMS;

  smop_const_list_proxyscalar_dispatcher = (SMOP__CORE__ScalarDispatcher*)smop_value_alloc(sizeof(SMOP__CORE__ScalarDispatcher));
  smop_const_list_proxyscalar_dispatcher->dispatcher = smop_const_ident_dispatcher;
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_ident_dispatcher);
  smop_const_list_proxyscalar_dispatcher->DESTR = &list_proxyscalar_dispatcher_DESTR;
  smop_const_list_proxyscalar_dispatcher->FETCH = &list_proxyscalar_dispatcher_FETCH;
  smop_const_list_proxyscalar_dispatcher->STORE = &list_proxyscalar_dispatcher_STORE;
  smop_const_list_proxyscalar_dispatcher->WHICH = &list_proxyscalar_dispatcher_WHICH;

}

SMOP__CORE__List* smop_list_create() {
  SMOP__CORE__List* foo = (SMOP__CORE__List*)smop_value_alloc(sizeof(SMOP__CORE__List));
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_list_dispatcher);
  foo->dispatcher = smop_const_list_dispatcher;
  return foo;
}

void smop_list_dispatcher_destr() {
  smop_value_refcnt_dec((SMOP__CORE__Value*)smop_const_list_dispatcher);
  smop_value_refcnt_dec((SMOP__CORE__Value*)smop_const_list_proxyscalar_dispatcher);
}
