#include "vroom.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct VROOM__CORE__List__ProxyScalar {
  VROOM__BASE__Value
  VROOM__CORE__ScalarDispatcher* dispatcher;
  VROOM__CORE__Value* cell;
  VROOM__CORE__List* owner;
  VROOM__CORE__int* index;
} VROOM__CORE__List__ProxyScalar;

VROOM__CORE__ScalarDispatcher* vroom_const_list_proxyscalar_dispatcher;

VROOM__CORE__List__ProxyScalar* vroom_list_proxyscalar_create() {
  VROOM__CORE__List__ProxyScalar* foo = (VROOM__CORE__List__ProxyScalar*)vroom_value_alloc(sizeof(VROOM__CORE__List__ProxyScalar));
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_list_proxyscalar_dispatcher);
  foo->dispatcher = (VROOM__CORE__ScalarDispatcher*)vroom_const_list_proxyscalar_dispatcher;
  return foo;
}




static void list_dispatcher_DESTR(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value) {
  vroom_value_wrlock(value);
  VROOM__CORE__List* list = (VROOM__CORE__List*)value;
  int length = list->length;
  VROOM__CORE__Value** items = list->items;
  list->length = 0;
  list->items = NULL;
  vroom_value_unlock(value);
  int i;
  for (i = 0; i < length; i++) {
    if (items[i]) {
      vroom_value_refcnt_dec(items[i]);
      items[i] = NULL;
    }
  }
  free(items);
}

static VROOM__CORE__Scalar* list_dispatcher_LOOKP(VROOM__CORE__Dispatcher* self,
                                                 VROOM__CORE__Value* v_value,
                                                 VROOM__CORE__int* index) {
  VROOM__CORE__List* value = (VROOM__CORE__List*)v_value;
  int wanted = vroom_int_lowlevel(index);
  vroom_value_wrlock(v_value);
  VROOM__CORE__Scalar* ret = NULL;
  if (value->length > wanted && value->items[wanted]) {
    ret = (VROOM__CORE__Scalar*)value->items[wanted];
    vroom_value_refcnt_inc((VROOM__CORE__Value*)ret);
    vroom_value_unlock(v_value);
  } else {
    vroom_value_unlock(v_value);
    VROOM__CORE__List__ProxyScalar* p = vroom_list_proxyscalar_create();
    // no need to lock here...
    p->cell = vroom_const_undef;
    vroom_value_refcnt_inc(vroom_const_undef);
    p->index = index;
    vroom_value_refcnt_inc((VROOM__CORE__Value*)index);
    p->owner = value;
    vroom_value_refcnt_inc(v_value);
    ret = (VROOM__CORE__Scalar*)p;
  }
  return ret;
}

static VROOM__CORE__Scalar* list_dispatcher_EXIST(VROOM__CORE__Dispatcher* self,
                                                 VROOM__CORE__Value* v_value,
                                                 VROOM__CORE__int* index) {
  int wanted = vroom_int_lowlevel(index);
  vroom_value_wrlock(v_value);
  VROOM__CORE__List* value = (VROOM__CORE__List*)v_value;
  VROOM__CORE__Scalar* ret = NULL;
  if (value->length > wanted) {
    VROOM__CORE__Scalar* ret = (VROOM__CORE__Scalar*)value->items[wanted];
    vroom_value_refcnt_inc((VROOM__CORE__Value*)ret);
  }
  vroom_value_unlock(v_value);
  return ret;
}

static VROOM__CORE__Scalar* list_dispatcher_DELET(VROOM__CORE__Dispatcher* self,
                                                 VROOM__CORE__Value* v_value,
                                                 VROOM__CORE__int* index) {
  int wanted = vroom_int_lowlevel(index);
  vroom_value_wrlock(v_value);
  VROOM__CORE__List* value = (VROOM__CORE__List*)v_value;
  VROOM__CORE__Scalar* ret = NULL;
  if (value->length >= wanted) {
    ret = (VROOM__CORE__Scalar*)value->items[wanted];
    value->items[wanted] = vroom_const_undef;
    vroom_value_refcnt_inc(vroom_const_undef);
  }
  vroom_value_unlock(v_value);
  return ret;
}

static VROOM__CORE__bytes* list_dispatcher_WHICH(VROOM__CORE__Dispatcher* self,
                                   VROOM__CORE__Value* value) {
  char str[32];
  sprintf(str, "list:%p", value);
  int len = strlen(str);
  return vroom_bytes_create(str, len);

}

static VROOM__CORE__int* list_dispatcher_ELEMS(VROOM__CORE__Dispatcher* self,
                                   VROOM__CORE__Value* value) {
  vroom_value_rdlock(value);
  int length = ((VROOM__CORE__List*)value)->length;
  vroom_value_unlock(value);
  return vroom_int_create(length);

}


static void list_proxyscalar_dispatcher_DESTR(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value) {
  vroom_value_wrlock(value);
  VROOM__CORE__Value* cell = ((VROOM__CORE__Scalar*)value)->cell;
  VROOM__CORE__List* owner = ((VROOM__CORE__List__ProxyScalar*)value)->owner;
  ((VROOM__CORE__Scalar*)value)->cell = NULL;
  ((VROOM__CORE__List__ProxyScalar*)value)->owner = NULL;
  vroom_value_unlock(value);
  if (cell) {
    vroom_value_refcnt_dec(cell);
  }
  if (owner) {
    vroom_value_refcnt_dec((VROOM__CORE__Value*)owner);
  }
}


static VROOM__CORE__Value* list_proxyscalar_dispatcher_FETCH(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value,
                                          VROOM__CORE__Value* wants) {

  vroom_value_wrlock(value);
  VROOM__CORE__Value* val = ((VROOM__CORE__Scalar*)value)->cell;
  vroom_value_unlock(value);
  vroom_value_refcnt_inc(val);
  return val;
}

static VROOM__CORE__Value* list_proxyscalar_dispatcher_STORE(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value,
                                          VROOM__CORE__Value* newvalue) {
  vroom_value_wrlock(value);
  VROOM__CORE__Value* oldval = ((VROOM__CORE__Scalar*)value)->cell;
  ((VROOM__CORE__Scalar*)value)->cell = newvalue;
  VROOM__CORE__List* list = ((VROOM__CORE__List__ProxyScalar*)value)->owner;
  VROOM__CORE__int* index_i = ((VROOM__CORE__List__ProxyScalar*)value)->index;
  int index = vroom_int_lowlevel(index_i);
  vroom_value_unlock(value);
  if (list) {
    vroom_value_wrlock((VROOM__CORE__Value*)list);
    if (list->length <= index) {
      list->items = (VROOM__CORE__Value**)realloc(list->items, sizeof(void*)*(index+1));
      assert(list->items);
      memset(list->items[list->length - 1], 0, list->length - index);
      list->length = index + 1;
    }
    if (list->items[index] == NULL) {
      list->items[index] = value;
    }
    vroom_value_refcnt_inc(value);
    vroom_value_unlock((VROOM__CORE__Value*)list);
    vroom_value_wrlock(value);
    ((VROOM__CORE__List__ProxyScalar*)value)->owner = NULL;
    vroom_value_refcnt_dec((VROOM__CORE__Value*)((VROOM__CORE__List__ProxyScalar*)value)->index);
    ((VROOM__CORE__List__ProxyScalar*)value)->index = NULL;
    vroom_value_refcnt_dec((VROOM__CORE__Value*)list);
    vroom_value_unlock(value);
  }
  vroom_value_refcnt_inc(newvalue);
  return oldval;
}

static VROOM__CORE__bytes* list_proxyscalar_dispatcher_WHICH(VROOM__CORE__Dispatcher* self,
                                   VROOM__CORE__Value* value) {
  vroom_value_wrlock(value);
  VROOM__CORE__Value* val = ((VROOM__CORE__Scalar*)value)->cell;
  vroom_value_unlock(value);
  return VROOM_WHICH(val);
}


VROOM__CORE__ListDispatcher* vroom_const_list_dispatcher;

void vroom_list_dispatcher_init() {
  vroom_const_list_dispatcher = (VROOM__CORE__ListDispatcher*)vroom_value_alloc(sizeof(VROOM__CORE__ListDispatcher));
  vroom_const_list_dispatcher->dispatcher = vroom_const_ident_dispatcher;
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_ident_dispatcher);
  vroom_const_list_dispatcher->DESTR = &list_dispatcher_DESTR;
  vroom_const_list_dispatcher->LOOKP = &list_dispatcher_LOOKP;
  vroom_const_list_dispatcher->EXIST = &list_dispatcher_EXIST;
  vroom_const_list_dispatcher->DELET = &list_dispatcher_DELET;
  vroom_const_list_dispatcher->WHICH = &list_dispatcher_WHICH;
  vroom_const_list_dispatcher->ELEMS = &list_dispatcher_ELEMS;

  vroom_const_list_proxyscalar_dispatcher = (VROOM__CORE__ScalarDispatcher*)vroom_value_alloc(sizeof(VROOM__CORE__ScalarDispatcher));
  vroom_const_list_proxyscalar_dispatcher->dispatcher = vroom_const_ident_dispatcher;
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_ident_dispatcher);
  vroom_const_list_proxyscalar_dispatcher->DESTR = &list_proxyscalar_dispatcher_DESTR;
  vroom_const_list_proxyscalar_dispatcher->FETCH = &list_proxyscalar_dispatcher_FETCH;
  vroom_const_list_proxyscalar_dispatcher->STORE = &list_proxyscalar_dispatcher_STORE;
  vroom_const_list_proxyscalar_dispatcher->WHICH = &list_proxyscalar_dispatcher_WHICH;

}

VROOM__CORE__List* vroom_list_create() {
  VROOM__CORE__List* foo = (VROOM__CORE__List*)vroom_value_alloc(sizeof(VROOM__CORE__List));
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_list_dispatcher);
  foo->dispatcher = vroom_const_list_dispatcher;
  return foo;
}

void vroom_list_dispatcher_destr() {
  vroom_value_refcnt_dec((VROOM__CORE__Value*)vroom_const_list_dispatcher);
  vroom_value_refcnt_dec((VROOM__CORE__Value*)vroom_const_list_proxyscalar_dispatcher);
}
