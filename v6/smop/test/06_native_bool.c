#include <stdio.h>
#include <smop.h>

int main() {

  printf("1..8\n");
  
  smop_init();

  SMOP__Object* true = SMOP__NATIVE__bool_create(1);
  if (!true) printf("not ");
  printf("ok 1 - can create a native bool value.\n");

  SMOP__Object* false = SMOP__NATIVE__bool_create(0);
  if (!false) printf("not ");
  printf("ok 2 - can create a native bool value.\n");

  SMOP__Object* true_again = SMOP__NATIVE__bool_create(1);
  if (!true_again) printf("not ");
  printf("ok 3 - can create a native bool value.\n");
  if (true_again != true) printf("not ");
  printf("ok 4 - bool are immortal constant values.\n");

  SMOP__Object* false_again = SMOP__NATIVE__bool_create(0);
  if (!false_again) printf("not ");
  printf("ok 5 - can create a native bool value.\n");
  if (false_again != false) printf("not ");
  printf("ok 6 - bool are immortal constant values.\n");

  int is_true = SMOP__NATIVE__bool_fetch(true);
  if (!is_true) printf("not ");
  printf("ok 7 - fetching true is true.\n");

  int is_false = SMOP__NATIVE__bool_fetch(false);
  if (is_false) printf("not ");
  printf("ok 8 - fetching false is false.\n");

  smop_destr();

  return 0;
}
