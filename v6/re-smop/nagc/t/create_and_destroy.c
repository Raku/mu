#include <stdlib.h>
#include <stdio.h>
#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>

/* In this test, we just want to see if we can create an nagc object
   and release it, seeing if it gets destroyed accordingly. */
static void custom_destroyall(SMOP__Object* interpreter,
                              SMOP__Object* value) {
  printf("ok 2 - Object destroyed.\n");
}
static SMOP__Object* placeholder(SMOP__Object* interpreter,
                                 SMOP__ResponderInterface* self,
                                 SMOP__Object* identifier,
                                 SMOP__Object* capture) {
  printf("unimplemented!\n");
  abort();
}


int main() {

  printf("1..3\n");

  SMOP__NAGC__ResponderInterface ri;
  ri.MESSAGE = placeholder;
  ri.REFERENCE = smop_nagc_reference;
  ri.RELEASE = smop_nagc_release;
  ri.WEAKREF = smop_nagc_weakref;
  ri.id = "test ri";
  ri.DESTROYALL = custom_destroyall;

  SMOP__Object* obj = smop_nagc_alloc(sizeof(SMOP__NAGC__Object));
  obj->RI = (SMOP__ResponderInterface*)&ri;

  printf("ok 1 - object created.\n");

  SMOP_RELEASE(SMOP__EmptyInterpreter, obj);

  printf("ok 3 - after object destruction\n");

  return 0;
}
