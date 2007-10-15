#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..4\n");

  yap6_init();

  YAP6__CORE__Value* foo = yap6_value_alloc(sizeof(YAP6__CORE__Value));

  if (foo->ref_cnt == 1) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 1 - refcnt starts in 1\n");

  yap6_value_refcnt_inc(foo);

  if (foo->ref_cnt == 2) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 2 - refcnt increment works\n");

  yap6_value_refcnt_dec(foo);
  if (foo->ref_cnt == 1) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 3 - refcnt decrement works\n");

  yap6_value_refcnt_dec(foo);
  printf("ok 4 - the pointer should be freed now...\n");


  return 0;
}
