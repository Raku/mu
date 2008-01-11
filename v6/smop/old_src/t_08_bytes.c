#include "smop.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char** argv) {
  printf("1..7\n");
  smop_init();

  SMOP__CORE__bytes* mybytes = smop_bytes_create("Hello, World!", 14);
  int sizeret = 0;

  if (strncmp(smop_bytes_lowlevel(mybytes,&sizeret),"Hello, World!",14) == 0) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 1 - init and lowlevel works...\n");

  if (sizeret == 14) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 2 - size matches...\n");

  SMOP__CORE__bytes* mywhich = SMOP_WHICH(mybytes);
  if (mybytes == mywhich) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 3 - WHICH on bytes returns the same object...\n");
  smop_value_refcnt_dec((SMOP__CORE__Value*)mywhich);

  SMOP__CORE__Value* bool = SMOP_BOOLN(mybytes);
  if (bool == smop_bool_false) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 4 - BOOLN is not false for a non-empty bytes...\n");
  smop_value_refcnt_dec((SMOP__CORE__Value*)bool);

  smop_value_refcnt_dec((SMOP__CORE__Value*)mybytes);
  printf("ok 5 - destroying the bytes...\n");

  mybytes = smop_bytes_create("", 0);
  bool = SMOP_BOOLN(mybytes);
  if (bool == smop_bool_false) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 6 - BOOLN is false for an empty bytes...\n");
  smop_value_refcnt_dec((SMOP__CORE__Value*)bool);

  smop_value_refcnt_dec((SMOP__CORE__Value*)mybytes);
  printf("ok 7 - destroying the bytes...\n");

  smop_destr();
  return 0;
}
