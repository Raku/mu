#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..3\n");
  printf("# will initialize...\n");

  yap6_init();
  printf("# initialized...\n");

  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  YAP6__CORE__List* mylist = yap6_list_create();
  
  printf("# list created...\n");
  // Let's get the proxy value for autovivifying some element.
  YAP6__CORE__int* index_0 = yap6_int_create(0);
  printf("# index object created...\n");
  YAP6__CORE__Scalar* item = YAP6_LIST_LOOKP(mylist,index_0);

  if (item) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("1 - autovivification is a two-step process.\n");

  // At this time, no item should exist...
  if (YAP6_LIST_EXIST(mylist,index_0) == NULL) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("2 - LOOKP should not yet autovivify the item...\n");

  // At this time, the item shall be created.
  YAP6__CORE__int* value = yap6_int_create(1234);
  YAP6_SCALAR_STORE(item, value);
  yap6_value_refcnt_dec((YAP6__CORE__Value*)value);

  item = YAP6_LIST_LOOKP(mylist,index_0);
  yap6_value_refcnt_dec((YAP6__CORE__Value*)index_0);
  // item is a scalar, we need to FETCH
  YAP6__CORE__int* result = (YAP6__CORE__int*)YAP6_SCALAR_FETCH(item, NULL);

  if (yap6_int_lowlevel(result) == 1234) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("3 - Lazy STORE should autovivify the item\n");

  return 0;
}
