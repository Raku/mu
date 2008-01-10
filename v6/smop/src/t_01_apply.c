#include "vroom.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..2\n");

  vroom_init();

  //if (VROOM_APPLY(vroom_const_undef, NULL, NULL) == vroom_const_undef) {
  //  printf("ok");
  //} else {
  //  printf("not ok");
  //}
  printf("not ok 1 - ident dispatcher APPLY returns the object itself # TODO\n");

  //if ((VROOM__CORE__Dispatcher*)VROOM_APPLY(vroom_const_ident_dispatcher, NULL, NULL) == vroom_const_ident_dispatcher) {
  //  printf("ok");
  //} else {
  //  printf("not ok");
  //}
  printf("not ok 2 - apply on value with no dispatcher means the value is a dispatcher # TODO\n");

  vroom_destr();
  return 0;
}
