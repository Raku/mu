#include <stdlib.h>
#include <stdio.h>
#include <smop/capture.h>
#include <smop/s0native.h>
int main() {
  printf("1..3\n");
  smop_s0native_init();
  smop_nagc_init();
  smop_capture_init();

  SMOP__Object* capture = SMOP__NATIVE__capture_create(SMOP__EmptyInterpreter,(SMOP__Object*[]) {SMOP__NATIVE__bool_false,SMOP__NATIVE__bool_true,NULL},(SMOP__Object*[]) {NULL});

  printf("ok 1\n");
  if (SMOP__NATIVE__capture_positional(SMOP__EmptyInterpreter,capture,0) == SMOP__NATIVE__bool_false) {
    printf("ok 2\n");
  } else {
    printf("not ok 2\n");
  }
  if (SMOP__NATIVE__capture_positional(SMOP__EmptyInterpreter,capture,1) == SMOP__NATIVE__bool_true) {
    printf("ok 3\n");
  } else {
    printf("not ok 3\n");
  }
  SMOP_RELEASE(SMOP__EmptyInterpreter,capture);

  smop_capture_destr();
  smop_nagc_destr();
  smop_s0native_destr();
  return 0;
}

