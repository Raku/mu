#include "vroom.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..4\n");
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


  VROOM__CORE__bytes* which = VROOM_WHICH(myint);
  if (which) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 2 - WHICH should return a bytes value...\n");

  int retsize = 0;
  char* val = vroom_bytes_lowlevel(which,&retsize);

  if (strncmp(val,"1234",4) == 0) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 3 - WHICH returns the sprintf representation of the int...\n");


  vroom_value_refcnt_dec((VROOM__CORE__Value*)which);
  vroom_value_refcnt_dec((VROOM__CORE__Value*)myint);

  printf("ok 4 - destroying the int...\n");

  vroom_destr();
  return 0;
}
