#include <smop.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..2\n");
  smop_init();

  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  SMOP__Object* myint = SMOP__NATIVE__int_create(1234);
  if (!myint) printf("not ");
  printf("ok 1 - create works...\n");

  if (SMOP__NATIVE__int_fetch(myint) != 1234) printf("not ");
  printf("ok 2 - fetch works...\n");

  SMOP_RELEASE(SMOP__INTPTR__InterpreterInstance, myint);

  smop_destr();
  return 0;
}
