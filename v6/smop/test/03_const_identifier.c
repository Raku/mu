
#include <smop.h>
#include <smop_lowlevel.h>

/* This test will create a constant identifier using a lowlevel C
 * string. For now, this type can only be used for identity checking
 * (pointer-identity), but in the future, it should implement the
 * stringification needed to the non-optimized loopkup.
 */

int main(int argc, char** argv) {
  printf("1..2\n");

  smop_init();

  SMOP__Object* mine = smop_native_idconst_create("hello");

  if (!mine) {
    printf("not ");
  }
  printf("ok 1 - creates an object successfully.\n");

  SMOP_RELEASE(NULL,mine);
  printf("ok 2 - release should work, even if the object is not subject to gc.\n");

  smop_destr();

  return 0;
}
