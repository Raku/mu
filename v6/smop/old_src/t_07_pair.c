#include "smop.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..5\n");

  smop_init();

  SMOP__CORE__int* key = smop_int_create(123);
  SMOP__CORE__int* val = smop_int_create(234);
  SMOP__CORE__Pair* pair = smop_pair_create((SMOP__CORE__Value*)key,(SMOP__CORE__Value*)val);
  smop_value_refcnt_dec((SMOP__CORE__Value*)key); key = NULL;
  smop_value_refcnt_dec((SMOP__CORE__Value*)val); val = NULL;

  if (pair) {
    printf("ok 1 ");
  } else {
    printf("not ok 1 ");
  }
  printf("- Pair initialized\n");

  key = (SMOP__CORE__int*)SMOP_PAIR_GTKEY(pair);
  if (smop_int_lowlevel(key) == 123) {
    printf("ok 2 ");
  } else {
    printf("not ok 2 ");
  }
  printf("- Get the key of a pair\n");
  smop_value_refcnt_dec((SMOP__CORE__Value*)key); key = NULL;

  val = (SMOP__CORE__int*)SMOP_PAIR_GTVAL(pair);
  if (smop_int_lowlevel(val) == 234) {
    printf("ok 3 ");
  } else {
    printf("not ok 3 ");
  }
  printf("- Get the value of a pair\n");
  smop_value_refcnt_dec((SMOP__CORE__Value*)val); val = NULL;

  SMOP__CORE__int* other = smop_int_create(345);
  val = (SMOP__CORE__int*)SMOP_PAIR_STVAL(pair,other);
  smop_value_refcnt_dec((SMOP__CORE__Value*)other); other = NULL;
  if (smop_int_lowlevel(val) == 234) {
    printf("ok 4 ");
  } else {
    printf("not ok 4 ");
  }
  printf("- Set returns the old value of a pair\n");
  smop_value_refcnt_dec((SMOP__CORE__Value*)val); val = NULL;

  val = (SMOP__CORE__int*)SMOP_PAIR_GTVAL(pair);
  if (smop_int_lowlevel(val) == 345) {
    printf("ok 5 ");
  } else {
    printf("not ok 5 ");
  }
  printf("- Get after set returns the correct value of a pair\n");
  smop_value_refcnt_dec((SMOP__CORE__Value*)val); val = NULL;
  smop_value_refcnt_dec((SMOP__CORE__Value*)pair); pair = NULL;

  smop_destr();

  return 0;
}
