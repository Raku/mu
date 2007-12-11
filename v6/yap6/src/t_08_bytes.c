#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char** argv) {
  printf("1..3\n");
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

  yap6_value_refcnt_dec((YAP6__CORE__Value*)mybytes);

  printf("ok 3 - destroying the bytes...\n");

  yap6_destr();
  return 0;
}
