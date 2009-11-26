#include <smop/s0native.h>
#include <stdio.h>
int main() {
  printf("1..1\n");
  smop_s0native_init();
  if (SMOP__EmptyInterpreter) {
    printf("ok 1\n");
  } else {
    printf("not ok 1\n");
  }
  smop_s0native_destr();
  return 0;
}
