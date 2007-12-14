#include "yap6.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

YAP6__CORE__List__ProxyScalar* yap6_list_proxyscalar_create() {
  YAP6__CORE__List__ProxyScalar* foo = (YAP6__CORE__List__ProxyScalar*)yap6_value_alloc(sizeof(YAP6__CORE__List__ProxyScalar));
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_list_proxyscalar_dispatcher);
  foo->dispatcher = (YAP6__CORE__ScalarDispatcher*)yap6_const_list_proxyscalar_dispatcher;
  return foo;
}


static YAP6__CORE__Value* list_dispatcher_APPLY(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__List* arguments,
                                          YAP6__CORE__Value* wants) {
  // TODO
  return NULL;
}


static void list_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
  YAP6__CORE__List* list = (YAP6__CORE__List*)value;
  int length = list->length;
  YAP6__CORE__Value** items = list->items;
  list->length = 0;
  list->items = NULL;
  yap6_value_unlock(value);
  int i;
  for (i = 0; i < length; i++) {
    if (items[i]) {
      yap6_value_refcnt_dec(items[i]);
      items[i] = NULL;
    }
  }
  free(items);
}

static YAP6__CORE__Scalar* list_dispatcher_LOOKP(YAP6__CORE__Dispatcher* self,
                                                 YAP6__CORE__Value* v_value,
                                                 YAP6__CORE__int* index) {
  YAP6__CORE__List* value = (YAP6__CORE__List*)v_value;
  int wanted = yap6_int_lowlevel(index);
  yap6_value_wrlock(v_value);
  YAP6__CORE__Scalar* ret = NULL;
  if (value->length > wanted && value->items[wanted]) {
    ret = (YAP6__CORE__Scalar*)value->items[wanted];
    yap6_value_refcnt_inc((YAP6__CORE__Value*)ret);
    yap6_value_unlock(v_value);
  } else {
    yap6_value_unlock(v_value);
    YAP6__CORE__List__ProxyScalar* p = yap6_list_proxyscalar_create();
    // no need to lock here...
    p->cell = yap6_const_undef;
    yap6_value_refcnt_inc(yap6_const_undef);
    p->index = index;
    yap6_value_refcnt_inc((YAP6__CORE__Value*)index);
    p->owner = value;
    yap6_value_refcnt_inc(v_value);
    ret = (YAP6__CORE__Scalar*)p;
  }
  return ret;
}

static YAP6__CORE__Scalar* list_dispatcher_EXIST(YAP6__CORE__Dispatcher* self,
                                                 YAP6__CORE__Value* v_value,
                                                 YAP6__CORE__int* index) {
  int wanted = yap6_int_lowlevel(index);
  yap6_value_wrlock(v_value);
  YAP6__CORE__List* value = (YAP6__CORE__List*)v_value;
  YAP6__CORE__Scalar* ret = NULL;
  if (value->length > wanted) {
    YAP6__CORE__Scalar* ret = (YAP6__CORE__Scalar*)value->items[wanted];
    yap6_value_refcnt_inc((YAP6__CORE__Value*)ret);
  }
  yap6_value_unlock(v_value);
  return ret;
}

static YAP6__CORE__Scalar* list_dispatcher_DELET(YAP6__CORE__Dispatcher* self,
                                                 YAP6__CORE__Value* v_value,
                                                 YAP6__CORE__int* index) {
  int wanted = yap6_int_lowlevel(index);
  yap6_value_wrlock(v_value);
  YAP6__CORE__List* value = (YAP6__CORE__List*)v_value;
  YAP6__CORE__Scalar* ret = NULL;
  if (value->length >= wanted) {
    ret = (YAP6__CORE__Scalar*)value->items[wanted];
    value->items[wanted] = yap6_const_undef;
    yap6_value_refcnt_inc(yap6_const_undef);
  }
  yap6_value_unlock(v_value);
  return ret;
}

static YAP6__CORE__bytes* list_dispatcher_WHICH(YAP6__CORE__Dispatcher* self,
                                   YAP6__CORE__Value* value) {
  char str[32];
  sprintf(str, "list:%p", value);
  int len = strlen(str);
  return yap6_bytes_create(str, len);

}

static YAP6__CORE__Value* list_proxyscalar_dispatcher_APPLY(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__List* arguments,
                                          YAP6__CORE__Value* wants) {
  // TODO
  return NULL;
}

static void list_proxyscalar_dispatcher_DESTR(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
  YAP6__CORE__Value* cell = ((YAP6__CORE__Scalar*)value)->cell;
  YAP6__CORE__List* owner = ((YAP6__CORE__List__ProxyScalar*)value)->owner;
  ((YAP6__CORE__Scalar*)value)->cell = NULL;
  ((YAP6__CORE__List__ProxyScalar*)value)->owner = NULL;
  yap6_value_unlock(value);
  if (cell) {
    yap6_value_refcnt_dec(cell);
  }
  if (owner) {
    yap6_value_refcnt_dec((YAP6__CORE__Value*)owner);
  }
}


static YAP6__CORE__Value* list_proxyscalar_dispatcher_FETCH(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* wants) {

  yap6_value_wrlock(value);
  YAP6__CORE__Value* val = ((YAP6__CORE__Scalar*)value)->cell;
  yap6_value_unlock(value);
  yap6_value_refcnt_inc(val);
  return val;
}

static YAP6__CORE__Value* list_proxyscalar_dispatcher_STORE(YAP6__CORE__Dispatcher* self,
                                          YAP6__CORE__Value* value,
                                          YAP6__CORE__Value* newvalue) {
  yap6_value_wrlock(value);
  YAP6__CORE__Value* oldval = ((YAP6__CORE__Scalar*)value)->cell;
  ((YAP6__CORE__Scalar*)value)->cell = newvalue;
  YAP6__CORE__List* list = ((YAP6__CORE__List__ProxyScalar*)value)->owner;
  YAP6__CORE__int* index_i = ((YAP6__CORE__List__ProxyScalar*)value)->index;
  int index = yap6_int_lowlevel(index_i);
  yap6_value_unlock(value);
  if (list) {
    yap6_value_wrlock((YAP6__CORE__Value*)list);
    if (list->length <= index) {
      list->items = (YAP6__CORE__Value**)realloc(list->items, sizeof(void*)*(index+1));
      assert(list->items);
      memset(list->items[list->length - 1], 0, list->length - index);
      list->length = index + 1;
    }
    if (list->items[index] == NULL) {
      list->items[index] = value;
    }
    yap6_value_refcnt_inc(value);
    yap6_value_unlock((YAP6__CORE__Value*)list);
    yap6_value_wrlock(value);
    ((YAP6__CORE__List__ProxyScalar*)value)->owner = NULL;
    yap6_value_refcnt_dec((YAP6__CORE__Value*)((YAP6__CORE__List__ProxyScalar*)value)->index);
    ((YAP6__CORE__List__ProxyScalar*)value)->index = NULL;
    yap6_value_refcnt_dec((YAP6__CORE__Value*)list);
    yap6_value_unlock(value);
  }
  yap6_value_refcnt_inc(newvalue);
  return oldval;
}

static YAP6__CORE__bytes* list_proxyscalar_dispatcher_WHICH(YAP6__CORE__Dispatcher* self,
                                   YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
  YAP6__CORE__Value* val = ((YAP6__CORE__Scalar*)value)->cell;
  yap6_value_unlock(value);
  return YAP6_WHICH(val);
}


YAP6__CORE__ListDispatcher* yap6_const_list_dispatcher;
YAP6__CORE__ScalarDispatcher* yap6_const_list_proxyscalar_dispatcher;

void yap6_list_dispatcher_init() {
  yap6_const_list_dispatcher = (YAP6__CORE__ListDispatcher*)yap6_value_alloc(sizeof(YAP6__CORE__ListDispatcher));
  yap6_const_list_dispatcher->dispatcher = yap6_const_ident_dispatcher;
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_ident_dispatcher);
  yap6_const_list_dispatcher->APPLY = &list_dispatcher_APPLY;
  yap6_const_list_dispatcher->DESTR = &list_dispatcher_DESTR;
  yap6_const_list_dispatcher->LOOKP = &list_dispatcher_LOOKP;
  yap6_const_list_dispatcher->EXIST = &list_dispatcher_EXIST;
  yap6_const_list_dispatcher->DELET = &list_dispatcher_DELET;
  yap6_const_list_dispatcher->WHICH = &list_dispatcher_WHICH;

  yap6_const_list_proxyscalar_dispatcher = (YAP6__CORE__ScalarDispatcher*)yap6_value_alloc(sizeof(YAP6__CORE__ScalarDispatcher));
  yap6_const_list_proxyscalar_dispatcher->dispatcher = yap6_const_ident_dispatcher;
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_ident_dispatcher);
  yap6_const_list_proxyscalar_dispatcher->APPLY = &list_proxyscalar_dispatcher_APPLY;
  yap6_const_list_proxyscalar_dispatcher->DESTR = &list_proxyscalar_dispatcher_DESTR;
  yap6_const_list_proxyscalar_dispatcher->FETCH = &list_proxyscalar_dispatcher_FETCH;
  yap6_const_list_proxyscalar_dispatcher->STORE = &list_proxyscalar_dispatcher_STORE;
  yap6_const_list_proxyscalar_dispatcher->WHICH = &list_proxyscalar_dispatcher_WHICH;

}

YAP6__CORE__List* yap6_list_create() {
  YAP6__CORE__List* foo = (YAP6__CORE__List*)yap6_value_alloc(sizeof(YAP6__CORE__List));
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_list_dispatcher);
  foo->dispatcher = yap6_const_list_dispatcher;
  return foo;
}

void yap6_list_dispatcher_destr() {
  yap6_value_refcnt_dec((YAP6__CORE__Value*)yap6_const_list_dispatcher);
  yap6_value_refcnt_dec((YAP6__CORE__Value*)yap6_const_list_proxyscalar_dispatcher);
}
