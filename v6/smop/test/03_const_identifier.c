#include <stdlib.h>
#include <stdio.h>
#include <smop.h>
#include <smop_lowlevel.h>

/* This test will create a constant identifier using a lowlevel C
 * string. For now, this type can only be used for identity checking
 * (pointer-identity), but in the future, it should implement the
 * stringification needed to the non-optimized loopkup.
 */

int main(int argc, char** argv) {
  printf("1..6\n");

  smop_init();

  if (!SMOP__ID__DESTROYALL) {
    printf("not ");
  }
  printf("ok 1 - the builtin constants must be defined at this point.\n");

  SMOP__Object* mine = SMOP__NATIVE__idconst_create("^!hel");

  if (!mine) {
    printf("not ");
  }
  printf("ok 2 - creates an object successfully.\n");

  SMOP_RELEASE(NULL,mine);
  printf("ok 3 - release should work, even if the object is not subject to gc.\n");

  SMOP__Object* other = SMOP__NATIVE__idconst_create("^!hel");
  if (mine != other) printf("not ");
  printf("ok 4 - idconst_create should check for previously created matching constants.\n");

  other = SMOP__NATIVE__idconst_createn("^!hello world", 5);
  if (mine != other) printf("not ");
  printf("ok 5 - idconst_createn should also check for previously created matching constants, but trimming the string on the size.\n");

  other = SMOP__NATIVE__idconst_createn("^!hel\0", 6);
  if (mine == other) printf("not ");
  printf("ok 6 - \\0 may be part of a string, and it should be part of the compairision..\n");

  smop_destr();

  return 0;
}
