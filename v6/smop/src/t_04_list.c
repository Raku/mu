#include "smop.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..3\n");
  printf("# will initialize...\n");

  smop_init();
  printf("# initialized...\n");

  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  SMOP__CORE__List* mylist = smop_list_create();
  
  printf("# list created...\n");
  // Let's get the proxy value for autovivifying some element.
  SMOP__CORE__int* index_0 = smop_int_create(0);
  printf("# index object created...\n");
  SMOP__CORE__Scalar* item = SMOP_LIST_LOOKP(mylist,index_0);

  if (item) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("1 - autovivification is a two-step process.\n");

  // At this time, no item should exist...
  if (SMOP_LIST_EXIST(mylist,index_0) == NULL) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("2 - LOOKP should not yet autovivify the item...\n");

  // At this time, the item shall be created.
  SMOP__CORE__int* value = smop_int_create(1234);
  SMOP__CORE__Value* oldvalue = SMOP_SCALAR_STORE(item, value);
  smop_value_refcnt_dec(oldvalue);
  smop_value_refcnt_dec((SMOP__CORE__Value*)value);
  smop_value_refcnt_dec((SMOP__CORE__Value*)item);

  item = SMOP_LIST_LOOKP(mylist,index_0);
  // item is a scalar, we need to FETCH
  SMOP__CORE__int* result = (SMOP__CORE__int*)SMOP_SCALAR_FETCH(item, NULL);

  smop_value_refcnt_dec((SMOP__CORE__Value*)index_0);
  smop_value_refcnt_dec((SMOP__CORE__Value*)item);

  if (smop_int_lowlevel(result) == 1234) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("3 - Lazy STORE should autovivify the item\n");

  smop_value_refcnt_dec((SMOP__CORE__Value*)result);
  smop_value_refcnt_dec((SMOP__CORE__Value*)mylist);

  smop_destr();

  return 0;
}
