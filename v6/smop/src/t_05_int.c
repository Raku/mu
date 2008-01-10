#include "smop.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..5\n");
  smop_init();

  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  SMOP__CORE__int* myint = smop_int_create(1234);
  
  if (smop_int_lowlevel(myint) == 1234) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 1 - init and lowlevel works...\n");

  SMOP__CORE__Value* bool = SMOP_BOOLN(myint);
  if (bool == smop_bool_false) {
    printf("not ok");
  } else {
    printf("ok");
  }

  smop_value_refcnt_dec(bool);
  printf(" 2 - 1234 is not false...\n");

  smop_value_refcnt_dec((SMOP__CORE__Value*)myint);
  printf("ok 3 - destroying the int...\n");

  myint = smop_int_create(0);
  bool = SMOP_BOOLN(myint);
  if (bool == smop_bool_false) {
    printf("ok");
  } else {
    printf("not ok");
  }
  smop_value_refcnt_dec(bool);
  printf(" 4 - 0 is false...\n");

  smop_value_refcnt_dec((SMOP__CORE__Value*)myint);
  printf("ok 5 - destroying the int...\n");

  smop_destr();
  return 0;
}
