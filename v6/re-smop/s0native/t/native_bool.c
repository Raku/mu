#include <smop_s0native.h>
#include <stdio.h>
int main() {
  printf("1..1\n");
  smop_s0native_init();
  printf("ok 1\n");
  smop_s0native_destr();
}
