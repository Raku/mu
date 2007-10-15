#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..2\n");
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

  yap6_value_refcnt_dec((YAP6__CORE__Value*)myint);

  printf("ok 2 - destroying the int...\n");

  return 0;
}
