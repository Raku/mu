#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..2\n");
  yap6_init();

  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  YAP6__CORE__List* mylist = yap6_list_init();
  
  // Let's get the proxy value for autovivifying some element.
  YAP6__CORE__int* index_0 = yap6_int_create(0);
  YAP6__CORE__Scalar* item = YAP6_LIST_LOOKP(mylist,index_0,NULL);

  if (item) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("1 - autovivification is a two-step process.\n");

  // At this time, no item should exist...
  if (YAP6_LIST_EXIST(mylist,index_0,NULL) == NULL) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("2 - LOOKP should not yet autovivify the item...\n");

  // At this time, the item shall be created.
  YAP6__CORE__int* value = yap6_int_create(1234);
  YAP6_SCALAR_STORE(item, value);
  yap6_value_refcnt_dec(value);

  item = YAP6_LIST_LOOKP(mylist,index_0,NULL);
  yap6_value_refcnt_dec((YAP6__CORE__Value*)index_0);
  // item is a scalar, we need to FETCH
  YAP6__CORE__int* result = (YAP6__CORE__int*)YAP6_SCALAR_FETCH(item);

  if (yap6_int_lowlever(result) == 1234) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("3 - Lazy STORE should autovivify the item\n");

  return 0;
}
