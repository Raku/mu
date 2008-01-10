#include "vroom.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..5\n");
  vroom_init();

  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  VROOM__CORE__int* myint = vroom_int_create(1234);
  
  if (vroom_int_lowlevel(myint) == 1234) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 1 - init and lowlevel works...\n");

  VROOM__CORE__Value* bool = VROOM_BOOLN(myint);
  if (bool == vroom_bool_false) {
    printf("not ok");
  } else {
    printf("ok");
  }

  vroom_value_refcnt_dec(bool);
  printf(" 2 - 1234 is not false...\n");

  vroom_value_refcnt_dec((VROOM__CORE__Value*)myint);
  printf("ok 3 - destroying the int...\n");

  myint = vroom_int_create(0);
  bool = VROOM_BOOLN(myint);
  if (bool == vroom_bool_false) {
    printf("ok");
  } else {
    printf("not ok");
  }
  vroom_value_refcnt_dec(bool);
  printf(" 4 - 0 is false...\n");

  vroom_value_refcnt_dec((VROOM__CORE__Value*)myint);
  printf("ok 5 - destroying the int...\n");

  vroom_destr();
  return 0;
}
