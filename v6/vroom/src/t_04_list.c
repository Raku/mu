#include "vroom.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..3\n");
  printf("# will initialize...\n");

  vroom_init();
  printf("# initialized...\n");

  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  VROOM__CORE__List* mylist = vroom_list_create();
  
  printf("# list created...\n");
  // Let's get the proxy value for autovivifying some element.
  VROOM__CORE__int* index_0 = vroom_int_create(0);
  printf("# index object created...\n");
  VROOM__CORE__Scalar* item = VROOM_LIST_LOOKP(mylist,index_0);

  if (item) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("1 - autovivification is a two-step process.\n");

  // At this time, no item should exist...
  if (VROOM_LIST_EXIST(mylist,index_0) == NULL) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("2 - LOOKP should not yet autovivify the item...\n");

  // At this time, the item shall be created.
  VROOM__CORE__int* value = vroom_int_create(1234);
  VROOM__CORE__Value* oldvalue = VROOM_SCALAR_STORE(item, value);
  vroom_value_refcnt_dec(oldvalue);
  vroom_value_refcnt_dec((VROOM__CORE__Value*)value);
  vroom_value_refcnt_dec((VROOM__CORE__Value*)item);

  item = VROOM_LIST_LOOKP(mylist,index_0);
  // item is a scalar, we need to FETCH
  VROOM__CORE__int* result = (VROOM__CORE__int*)VROOM_SCALAR_FETCH(item, NULL);

  vroom_value_refcnt_dec((VROOM__CORE__Value*)index_0);
  vroom_value_refcnt_dec((VROOM__CORE__Value*)item);

  if (vroom_int_lowlevel(result) == 1234) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("3 - Lazy STORE should autovivify the item\n");

  vroom_value_refcnt_dec((VROOM__CORE__Value*)result);
  vroom_value_refcnt_dec((VROOM__CORE__Value*)mylist);

  vroom_destr();

  return 0;
}
