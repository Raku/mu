#include <smop/s0native.h>
#include <stdio.h>
int main() {
  printf("1..4\n");
  smop_s0native_init();
  printf("ok 1\n");
  if (SMOP__NATIVE__bool_true != SMOP__NATIVE__bool_false) {
    printf("ok 2\n");
  } else {
    printf("not ok 2\n");
  }
  if (SMOP__NATIVE__bool_true) {
    printf("ok 3\n");
  } else {
    printf("not ok 3\n");
  }
  if (SMOP__NATIVE__bool_false) {
    printf("ok 4\n");
  } else {
    printf("not ok 4\n");
  }
  smop_s0native_destr();
  return 0;
}
