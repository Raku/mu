#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..5\n");

  yap6_init();

  YAP6__CORE__int* key = yap6_int_create(123);
  YAP6__CORE__int* val = yap6_int_create(234);
  YAP6__CORE__Pair* pair = yap6_pair_create((YAP6__CORE__Value*)key,(YAP6__CORE__Value*)val);
  yap6_value_refcnt_dec((YAP6__CORE__Value*)key); key = NULL;
  yap6_value_refcnt_dec((YAP6__CORE__Value*)val); val = NULL;

  if (pair) {
    printf("ok 1 ");
  } else {
    printf("not ok 1 ");
  }
  printf("- Pair initialized\n");

  key = (YAP6__CORE__int*)YAP6_PAIR_GTKEY(pair);
  if (yap6_int_lowlevel(key) == 123) {
    printf("ok 2 ");
  } else {
    printf("not ok 2 ");
  }
  printf("- Get the key of a pair\n");
  yap6_value_refcnt_dec((YAP6__CORE__Value*)key); key = NULL;

  val = (YAP6__CORE__int*)YAP6_PAIR_GTVAL(pair);
  if (yap6_int_lowlevel(val) == 234) {
    printf("ok 3 ");
  } else {
    printf("not ok 3 ");
  }
  printf("- Get the value of a pair\n");
  yap6_value_refcnt_dec((YAP6__CORE__Value*)val); val = NULL;

  YAP6__CORE__int* other = yap6_int_create(345);
  val = (YAP6__CORE__int*)YAP6_PAIR_STVAL(pair,other);
  yap6_value_refcnt_dec((YAP6__CORE__Value*)other); other = NULL;
  if (yap6_int_lowlevel(val) == 234) {
    printf("ok 4 ");
  } else {
    printf("not ok 4 ");
  }
  printf("- Set returns the old value of a pair\n");
  yap6_value_refcnt_dec((YAP6__CORE__Value*)val); val = NULL;

  val = (YAP6__CORE__int*)YAP6_PAIR_GTVAL(pair);
  if (yap6_int_lowlevel(val) == 345) {
    printf("ok 5 ");
  } else {
    printf("not ok 5 ");
  }
  printf("- Get after set returns the correct value of a pair\n");
  yap6_value_refcnt_dec((YAP6__CORE__Value*)val); val = NULL;
  yap6_value_refcnt_dec((YAP6__CORE__Value*)pair); pair = NULL;

  yap6_destr();

  return 0;
}
