#include "vroom.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..4\n");

  vroom_init();

  VROOM__CORE__Value* foo = vroom_value_alloc(sizeof(VROOM__CORE__Value));
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_ident_dispatcher);
  foo->dispatcher = vroom_const_ident_dispatcher;

  if (foo->ref_cnt == 1) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 1 - refcnt starts in 1\n");

  vroom_value_refcnt_inc(foo);

  if (foo->ref_cnt == 2) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 2 - refcnt increment works\n");

  vroom_value_refcnt_dec(foo);
  if (foo->ref_cnt == 1) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 3 - refcnt decrement works\n");

  vroom_value_refcnt_dec(foo);
  printf("ok 4 - the pointer should be freed now...\n");


  vroom_destr();
  return 0;
}
