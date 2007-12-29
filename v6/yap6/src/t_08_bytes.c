#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char** argv) {
  printf("1..7\n");
  yap6_init();

  YAP6__CORE__bytes* mybytes = yap6_bytes_create("Hello, World!", 14);
  int sizeret = 0;

  if (strncmp(yap6_bytes_lowlevel(mybytes,&sizeret),"Hello, World!",14) == 0) {
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

  YAP6__CORE__bytes* mywhich = YAP6_WHICH(mybytes);
  if (mybytes == mywhich) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 3 - WHICH on bytes returns the same object...\n");
  yap6_value_refcnt_dec((YAP6__CORE__Value*)mywhich);

  YAP6__CORE__Value* bool = YAP6_BOOLN(mybytes);
  if (bool == yap6_bool_false) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 4 - BOOLN is not false for a non-empty bytes...\n");
  yap6_value_refcnt_dec((YAP6__CORE__Value*)bool);

  yap6_value_refcnt_dec((YAP6__CORE__Value*)mybytes);
  printf("ok 5 - destroying the bytes...\n");

  mybytes = yap6_bytes_create("", 0);
  bool = YAP6_BOOLN(mybytes);
  if (bool == yap6_bool_false) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 6 - BOOLN is false for an empty bytes...\n");
  yap6_value_refcnt_dec((YAP6__CORE__Value*)bool);

  yap6_value_refcnt_dec((YAP6__CORE__Value*)mybytes);
  printf("ok 7 - destroying the bytes...\n");

  yap6_destr();
  return 0;
}
