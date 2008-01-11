#include "smop.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..4\n");
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


  SMOP__CORE__bytes* which = SMOP_WHICH(myint);
  if (which) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 2 - WHICH should return a bytes value...\n");

  int retsize = 0;
  char* val = smop_bytes_lowlevel(which,&retsize);

  if (strncmp(val,"1234",4) == 0) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 3 - WHICH returns the sprintf representation of the int...\n");


  smop_value_refcnt_dec((SMOP__CORE__Value*)which);
  smop_value_refcnt_dec((SMOP__CORE__Value*)myint);

  printf("ok 4 - destroying the int...\n");

  smop_destr();
  return 0;
}
