#include <stdlib.h>
#include <stdio.h>
#include <smop.h>
#include <smop_lowlevel.h>

/*
 * In this test we want to test only the smop lowlevel runtime, to do
 * that, we're going to fake a lot of features the lowlevel runtime
 * counts on. Note that it may look weird that the lowlevel runtime
 * requires some features that comes from the high-level, but it's
 * also important to realise that there's a matter of timing
 * also. Which basically means that we know that at the time we need
 * the high-level features, they will be already available.
 *
 * This test, even testing the lowlevel features of SMOP depends on
 * some features of the high-level smop.
 */

/* In this test, we're going to define a object that is its own
 * responder interface, and that have three methods (one of them being
 * DESTROYALL). Each one will print to the standard output the "ok"
 * message in the expected order.
 */
static SMOP__Object* custom_MESSAGE(SMOP__Object* stack,
                                    SMOP__ResponderInterface* self,
                                    SMOP__Object* identifier,
                                    SMOP__Object* capture) {
  if ((int)identifier == SMOP__ID__new) {
    printf("ok 3 - method 1 should be called early.\n");
  } else if ((int)identifier == SMOP__ID__invocant) {
    printf("ok 4 - method 2 should be called immediatly afterwards.\n");
  } else if (identifier == SMOP__ID__DESTROYALL) {
    printf("ok 5 - DESTROYALL should be the last one called.\n");
  } else {
    printf("not ok - Unknown identifier given %p.\n", identifier);
  }
  if (capture) SMOP_RELEASE(stack, capture);
  return NULL;
}

/* It works like:
 * 1 - boot smop,
 * 2 - init a new object, and set the methods
 * 3 - get a stack
 * 4 - call method 1
 * 5 - lower the refcount (should init destruction)
 * 6 - call method 2
 * 7 - call stack loop
 */
int main(int argc, char** argv) {
  printf("1..6\n");

  smop_init();

  SMOP__Object* obj = smop_lowlevel_alloc(sizeof(SMOP__ResponderInterface));
  if (!obj) {
    printf("not ");
  }
  printf("ok 1 - object allocated successfully.\n");

  SMOP__ResponderInterface* ri = (SMOP__ResponderInterface*)obj;
  ri->MESSAGE = custom_MESSAGE;
  ri->REFERENCE = smop_lowlevel_refcnt_inc;
  ri->RELEASE = smop_lowlevel_refcnt_dec;

  SMOP__Object* intrp = SMOP_DISPATCH(SMOP__INTPTR__InterpreterInstance,
                                      SMOP_RI(SMOP__INTPTR__InterpreterInstance),
                                      SMOP__ID__new,
                                      SMOP__NATIVE__capture_create(SMOP__INTPTR__InterpreterInstance,
                                                                   SMOP__INTPTR__InterpreterInstance, NULL, NULL));
  
  if (!intrp) {
    printf("not ");
  }
  printf("ok 2 - got new interp successfully %p.\n",intrp);

  SMOP_DISPATCH(intrp, ri, SMOP__ID__new, NULL);

  /* At this point, the destruction code for the object will be put in
   * the stack. That's why we still can call the second method just
   * below that. The object will only be invalidated when the stack
   * loop is called.
   */
  SMOP_RELEASE(intrp, obj);

  SMOP_DISPATCH(intrp, ri, SMOP__ID__invocant, NULL);

  SMOP_DISPATCH(intrp, SMOP_RI(intrp),
                SMOP__ID__loop, 
                SMOP__NATIVE__capture_create(intrp,
                                             SMOP_REFERENCE(intrp,intrp),
                                             NULL, NULL));

  SMOP_RELEASE(SMOP__INTPTR__InterpreterInstance, intrp);

  printf("ok 6 - finished succesfully.\n");

  smop_destr();

  return 0;
}


