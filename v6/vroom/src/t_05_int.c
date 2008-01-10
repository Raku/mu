#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..5\n");
  yap6_init();

  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  YAP6__CORE__int* myint = yap6_int_create(1234);
  
  if (yap6_int_lowlevel(myint) == 1234) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 1 - init and lowlevel works...\n");

  YAP6__CORE__Value* bool = YAP6_BOOLN(myint);
  if (bool == yap6_bool_false) {
    printf("not ok");
  } else {
    printf("ok");
  }

  yap6_value_refcnt_dec(bool);
  printf(" 2 - 1234 is not false...\n");

  yap6_value_refcnt_dec((YAP6__CORE__Value*)myint);
  printf("ok 3 - destroying the int...\n");

  myint = yap6_int_create(0);
  bool = YAP6_BOOLN(myint);
  if (bool == yap6_bool_false) {
    printf("ok");
  } else {
    printf("not ok");
  }
  yap6_value_refcnt_dec(bool);
  printf(" 4 - 0 is false...\n");

  yap6_value_refcnt_dec((YAP6__CORE__Value*)myint);
  printf("ok 5 - destroying the int...\n");

  yap6_destr();
  return 0;
}
