#include "smop.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..3\n");

  smop_init();
  printf("# initialized...\n");

  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  SMOP__CORE__int* value = smop_int_create(1234);
  SMOP__CORE__Scalar* scalar = smop_scalar_create((SMOP__CORE__Value*)value);
  smop_value_refcnt_dec((SMOP__CORE__Value*)value);
  value = NULL;
  
  SMOP__CORE__int* result = (SMOP__CORE__int*)SMOP_SCALAR_FETCH(scalar, NULL);
  if (smop_int_lowlevel(result) == 1234) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("1 - Fetch gets the right value\n");
  smop_value_refcnt_dec((SMOP__CORE__Value*)result);
  result = NULL;

  value = smop_int_create(2345);
  result = (SMOP__CORE__int*)SMOP_SCALAR_STORE(scalar, value);
  if (smop_int_lowlevel(result) == 1234) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("2 - Store returns the old value\n");
  smop_value_refcnt_dec((SMOP__CORE__Value*)result);
  result = NULL;
  smop_value_refcnt_dec((SMOP__CORE__Value*)value);
  value = NULL;

  result = (SMOP__CORE__int*)SMOP_SCALAR_FETCH(scalar, NULL);
  if (smop_int_lowlevel(result) == 2345) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("3 - Fetch gets the right value\n");
  smop_value_refcnt_dec((SMOP__CORE__Value*)result);
  result = NULL;

  smop_value_refcnt_dec((SMOP__CORE__Value*)scalar);
  scalar = NULL;

  smop_destr();

  return 0;
}
