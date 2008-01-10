#include "vroom.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char** argv) {
  printf("1..7\n");
  vroom_init();

  VROOM__CORE__bytes* mybytes = vroom_bytes_create("Hello, World!", 14);
  int sizeret = 0;

  if (strncmp(vroom_bytes_lowlevel(mybytes,&sizeret),"Hello, World!",14) == 0) {
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

  VROOM__CORE__bytes* mywhich = VROOM_WHICH(mybytes);
  if (mybytes == mywhich) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 3 - WHICH on bytes returns the same object...\n");
  vroom_value_refcnt_dec((VROOM__CORE__Value*)mywhich);

  VROOM__CORE__Value* bool = VROOM_BOOLN(mybytes);
  if (bool == vroom_bool_false) {
    printf("not ok");
  } else {
    printf("ok");
  }
  printf(" 4 - BOOLN is not false for a non-empty bytes...\n");
  vroom_value_refcnt_dec((VROOM__CORE__Value*)bool);

  vroom_value_refcnt_dec((VROOM__CORE__Value*)mybytes);
  printf("ok 5 - destroying the bytes...\n");

  mybytes = vroom_bytes_create("", 0);
  bool = VROOM_BOOLN(mybytes);
  if (bool == vroom_bool_false) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 6 - BOOLN is false for an empty bytes...\n");
  vroom_value_refcnt_dec((VROOM__CORE__Value*)bool);

  vroom_value_refcnt_dec((VROOM__CORE__Value*)mybytes);
  printf("ok 7 - destroying the bytes...\n");

  vroom_destr();
  return 0;
}
