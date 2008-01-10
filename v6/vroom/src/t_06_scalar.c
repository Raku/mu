#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..3\n");

  yap6_init();
  printf("# initialized...\n");

  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  YAP6__CORE__int* value = yap6_int_create(1234);
  YAP6__CORE__Scalar* scalar = yap6_scalar_create((YAP6__CORE__Value*)value);
  yap6_value_refcnt_dec((YAP6__CORE__Value*)value);
  value = NULL;
  
  YAP6__CORE__int* result = (YAP6__CORE__int*)YAP6_SCALAR_FETCH(scalar, NULL);
  if (yap6_int_lowlevel(result) == 1234) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("1 - Fetch gets the right value\n");
  yap6_value_refcnt_dec((YAP6__CORE__Value*)result);
  result = NULL;

  value = yap6_int_create(2345);
  result = (YAP6__CORE__int*)YAP6_SCALAR_STORE(scalar, value);
  if (yap6_int_lowlevel(result) == 1234) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("2 - Store returns the old value\n");
  yap6_value_refcnt_dec((YAP6__CORE__Value*)result);
  result = NULL;
  yap6_value_refcnt_dec((YAP6__CORE__Value*)value);
  value = NULL;

  result = (YAP6__CORE__int*)YAP6_SCALAR_FETCH(scalar, NULL);
  if (yap6_int_lowlevel(result) == 2345) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("3 - Fetch gets the right value\n");
  yap6_value_refcnt_dec((YAP6__CORE__Value*)result);
  result = NULL;

  yap6_value_refcnt_dec((YAP6__CORE__Value*)scalar);
  scalar = NULL;

  yap6_destr();

  return 0;
}
