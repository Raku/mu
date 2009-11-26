
#include <stdlib.h>
#include <stdio.h>
#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>

/* In this test, we just want to see if we can create an nagc object
   and release it, seeing if it gets destroyed accordingly. */
static void custom_destroyall(SMOP__Object* interpreter,
                              SMOP__Object* value) {
  printf("ok 4 - Object destroyed.\n");
}

int main() {

  printf("1..6\n");

  smop_s0native_init();
  smop_nagc_init();

  SMOP__NAGC__ResponderInterface ri;
  ri.RI = NULL;
  ri.MESSAGE = smop_placeholder_message;
  ri.REFERENCE = smop_nagc_reference;
  ri.RELEASE = smop_nagc_release;
  ri.WEAKREF = smop_nagc_weakref;
  ri.id = "test ri";
  ri.DESTROYALL = custom_destroyall;

  SMOP__Object* obj = smop_nagc_alloc(sizeof(SMOP__NAGC__Object));
  obj->RI = (SMOP__ResponderInterface*)&ri;

  printf("ok 1 - object created.\n");

  SMOP__Object* ref = SMOP_WEAKREF(SMOP__EmptyInterpreter, obj);

  if (!ref) {
    printf("not ");
  }
  printf("ok 2 - weakref created.\n");

  if (((SMOP__NAGC__WeakRef*)ref)->ref != (SMOP__NAGC__Object*)obj) {
    printf("not ");
  }
  printf("ok 3 - weakref points to object.\n");

  SMOP_RELEASE(SMOP__EmptyInterpreter, obj);

  printf("ok 5 - after object destruction\n");

  if (((SMOP__Object*)(((SMOP__NAGC__WeakRef*)ref)->ref)) != SMOP__NATIVE__bool_false) {
    printf("not ");
  }
  printf("ok 6 - weakref points to false.\n");

  SMOP_RELEASE(SMOP__EmptyInterpreter, ref);

  smop_nagc_destr();
  smop_s0native_destr();

  return 0;
}
