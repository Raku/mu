#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..2\n");

  yap6_ident_dispatcher_init();
  yap6_const_init();

  if (YAP6_APPLY(yap6_const_undef, NULL, NULL) == yap6_const_undef) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 1 - ident dispatcher APPLY returns the object itself\n");

  if ((YAP6__CORE__Dispatcher*)YAP6_APPLY(yap6_const_ident_dispatcher, NULL, NULL) == yap6_const_ident_dispatcher) {
    printf("ok");
  } else {
    printf("not ok");
  }
  printf(" 2 - apply on value with no dispatcher means the value is a dispatcher\n");

  return 0;
}
