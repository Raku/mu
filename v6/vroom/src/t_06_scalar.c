#include "vroom.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..3\n");

  vroom_init();
  printf("# initialized...\n");

  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  VROOM__CORE__int* value = vroom_int_create(1234);
  VROOM__CORE__Scalar* scalar = vroom_scalar_create((VROOM__CORE__Value*)value);
  vroom_value_refcnt_dec((VROOM__CORE__Value*)value);
  value = NULL;
  
  VROOM__CORE__int* result = (VROOM__CORE__int*)VROOM_SCALAR_FETCH(scalar, NULL);
  if (vroom_int_lowlevel(result) == 1234) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("1 - Fetch gets the right value\n");
  vroom_value_refcnt_dec((VROOM__CORE__Value*)result);
  result = NULL;

  value = vroom_int_create(2345);
  result = (VROOM__CORE__int*)VROOM_SCALAR_STORE(scalar, value);
  if (vroom_int_lowlevel(result) == 1234) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("2 - Store returns the old value\n");
  vroom_value_refcnt_dec((VROOM__CORE__Value*)result);
  result = NULL;
  vroom_value_refcnt_dec((VROOM__CORE__Value*)value);
  value = NULL;

  result = (VROOM__CORE__int*)VROOM_SCALAR_FETCH(scalar, NULL);
  if (vroom_int_lowlevel(result) == 2345) {
    printf("ok ");
  } else {
    printf("not ok ");
  }
  printf("3 - Fetch gets the right value\n");
  vroom_value_refcnt_dec((VROOM__CORE__Value*)result);
  result = NULL;

  vroom_value_refcnt_dec((VROOM__CORE__Value*)scalar);
  scalar = NULL;

  vroom_destr();

  return 0;
}
