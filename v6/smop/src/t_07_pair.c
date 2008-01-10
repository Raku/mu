#include "vroom.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..5\n");

  vroom_init();

  VROOM__CORE__int* key = vroom_int_create(123);
  VROOM__CORE__int* val = vroom_int_create(234);
  VROOM__CORE__Pair* pair = vroom_pair_create((VROOM__CORE__Value*)key,(VROOM__CORE__Value*)val);
  vroom_value_refcnt_dec((VROOM__CORE__Value*)key); key = NULL;
  vroom_value_refcnt_dec((VROOM__CORE__Value*)val); val = NULL;

  if (pair) {
    printf("ok 1 ");
  } else {
    printf("not ok 1 ");
  }
  printf("- Pair initialized\n");

  key = (VROOM__CORE__int*)VROOM_PAIR_GTKEY(pair);
  if (vroom_int_lowlevel(key) == 123) {
    printf("ok 2 ");
  } else {
    printf("not ok 2 ");
  }
  printf("- Get the key of a pair\n");
  vroom_value_refcnt_dec((VROOM__CORE__Value*)key); key = NULL;

  val = (VROOM__CORE__int*)VROOM_PAIR_GTVAL(pair);
  if (vroom_int_lowlevel(val) == 234) {
    printf("ok 3 ");
  } else {
    printf("not ok 3 ");
  }
  printf("- Get the value of a pair\n");
  vroom_value_refcnt_dec((VROOM__CORE__Value*)val); val = NULL;

  VROOM__CORE__int* other = vroom_int_create(345);
  val = (VROOM__CORE__int*)VROOM_PAIR_STVAL(pair,other);
  vroom_value_refcnt_dec((VROOM__CORE__Value*)other); other = NULL;
  if (vroom_int_lowlevel(val) == 234) {
    printf("ok 4 ");
  } else {
    printf("not ok 4 ");
  }
  printf("- Set returns the old value of a pair\n");
  vroom_value_refcnt_dec((VROOM__CORE__Value*)val); val = NULL;

  val = (VROOM__CORE__int*)VROOM_PAIR_GTVAL(pair);
  if (vroom_int_lowlevel(val) == 345) {
    printf("ok 5 ");
  } else {
    printf("not ok 5 ");
  }
  printf("- Get after set returns the correct value of a pair\n");
  vroom_value_refcnt_dec((VROOM__CORE__Value*)val); val = NULL;
  vroom_value_refcnt_dec((VROOM__CORE__Value*)pair); pair = NULL;

  vroom_destr();

  return 0;
}
