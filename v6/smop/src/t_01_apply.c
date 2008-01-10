#include "smop.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..2\n");

  smop_init();

  //if (SMOP_APPLY(smop_const_undef, NULL, NULL) == smop_const_undef) {
  //  printf("ok");
  //} else {
  //  printf("not ok");
  //}
  printf("not ok 1 - ident dispatcher APPLY returns the object itself # TODO\n");

  //if ((SMOP__CORE__Dispatcher*)SMOP_APPLY(smop_const_ident_dispatcher, NULL, NULL) == smop_const_ident_dispatcher) {
  //  printf("ok");
  //} else {
  //  printf("not ok");
  //}
  printf("not ok 2 - apply on value with no dispatcher means the value is a dispatcher # TODO\n");

  smop_destr();
  return 0;
}
