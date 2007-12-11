#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..4\n");
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


  YAP6__CORE__bytes* which = YAP6_WHICH(myint);
  if (which) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 2 - WHICH should return a bytes value...\n");

  int retsize = 0;
  char* val = yap6_bytes_lowlevel(which,&retsize);

  if (strncmp(val,"1234",4) == 0) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 3 - WHICH returns the sprintf representation of the int...\n");


  yap6_value_refcnt_dec((YAP6__CORE__Value*)which);
  yap6_value_refcnt_dec((YAP6__CORE__Value*)myint);

  printf("ok 4 - destroying the int...\n");

  yap6_destr();
  return 0;
}
