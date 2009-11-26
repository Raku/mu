#include <stdlib.h>
#include <stdio.h>
#include <smop/base.h>
#include <smop/s0native.h>

/* This test will create a constant identifier using a lowlevel C
 * string. For now, this type can only be used for identity checking
 * (pointer-identity), but in the future, it should implement the
 * stringification needed to the non-optimized loopkup.
 */

int main(int argc, char** argv) {
  printf("1..5\n");

  smop_s0native_init();


  SMOP__Object* mine = SMOP__NATIVE__idconst_createn("^!hel",5);

  if (!mine) {
    printf("not ");
  }
  printf("ok 1 - creates an object successfully.\n");

  SMOP_RELEASE(NULL,mine);
  printf("ok 2 - release should work, even if the object is not subject to gc.\n");

  SMOP__Object* other = SMOP__NATIVE__idconst_createn("^!hel",5);
  if (mine != other) printf("not ");
  printf("ok 3 - idconst_create should check for previously created matching constants.\n");

  other = SMOP__NATIVE__idconst_createn("^!hello world", 5);
  if (mine != other) printf("not ");
  printf("ok 4 - idconst_createn should also check for previously created matching constants, but trimming the string on the size.\n");

  other = SMOP__NATIVE__idconst_createn("^!hel\0", 6);
  if (mine == other) printf("not ");
  printf("ok 5 - \\0 may be part of a string, and it should be part of the compairision..\n");

  smop_s0native_destr();

  return 0;
}
