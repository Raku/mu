#include "smop.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..4\n");

  smop_init();

  SMOP__CORE__Value* foo = smop_value_alloc(sizeof(SMOP__CORE__Value));
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_ident_dispatcher);
  foo->dispatcher = smop_const_ident_dispatcher;

  if (foo->ref_cnt == 1) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 1 - refcnt starts in 1\n");

  smop_value_refcnt_inc(foo);

  if (foo->ref_cnt == 2) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 2 - refcnt increment works\n");

  smop_value_refcnt_dec(foo);
  if (foo->ref_cnt == 1) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 3 - refcnt decrement works\n");

  smop_value_refcnt_dec(foo);
  printf("ok 4 - the pointer should be freed now...\n");


  smop_destr();
  return 0;
}
