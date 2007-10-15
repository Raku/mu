#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

YAP6__CORE__Value* foo;

void threaded_function(void* unused) {
  // this will be the function used by t1, t2 and t3
  

  // the trick consists in incrementing the reference count when using
  // the value, so the value don't vanish in the meanwhile...
  yap6_value_refcnt_inc(foo);

  // let's sleep...
  sleep(1);
  sleep(1);

  yap6_value_refcnt_dec(foo);
  printf("ok - it didn't break while trying to use it\n");
}

void evil_function(void* unused) {
  // this will be the function used by t4
  // let's sleep...
  sleep(1);

  // this thread would destroy the value...
  yap6_value_refcnt_dec(foo);
  printf("ok - it didn't break while trying to use it\n");
}

int main(int argc, char** argv) {
  printf("1..5\n");

  yap6_init();

  foo = yap6_value_alloc(sizeof(YAP6__CORE__Value));

  if (foo->ref_cnt == 1) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" - refcnt starts in 1\n");

  pthread_t t1, t2, t3, t4;
  pthread_create(&t1, NULL, &threaded_function, NULL);
  pthread_create(&t2, NULL, &threaded_function, NULL);
  pthread_create(&t3, NULL, &threaded_function, NULL);
  pthread_create(&t4, NULL, &evil_function, NULL);

  pthread_join(t4, NULL);
  pthread_join(t3, NULL);
  pthread_join(t2, NULL);
  pthread_join(t1, NULL);

  return 0;
}
