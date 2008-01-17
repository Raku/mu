
#include <smop.h>
#include <smop_lowlevel.h>

/* This test will create a constant identifier using a lowlevel C
 * string. For now, this type can only be used for identity checking
 * (pointer-identity), but in the future, it should implement the
 * stringification needed to the non-optimized loopkup.
 */

int main(int argc, char** argv) {
  printf("1..4\n");

  smop_init();

  if (!SMOP__ID__DESTROYALL) {
    printf("not ");
  }
  printf("ok 1 - the builtin constants must be defined at this point.\n");

  SMOP__Object* mine = SMOP__NATIVE__idconst_create("hello");

  if (!mine) {
    printf("not ");
  }
  printf("ok 2 - creates an object successfully.\n");

  SMOP_RELEASE(NULL,mine);
  printf("ok 3 - release should work, even if the object is not subject to gc.\n");

  char* val = SMOP__NATIVE__idconst_fetch(mine);

  if (strcmp(val,"hello") != 0) {
    printf("not ");
  }
  printf("ok 4 - the value can be fetched in the lowlevel successfully.\n");

  free(val);

  smop_destr();

  return 0;
}
