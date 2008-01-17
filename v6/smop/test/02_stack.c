
#include <smop.h>
#include <smop_lowlevel.h>

/* This test will have a set of methods in an object called by the
 * stack.
 */

static SMOP__Object* method1;
static SMOP__Object* method2;
static SMOP__Object* method3;
static SMOP__Object* method4;

static SMOP__Object* custom_MESSAGE(SMOP__Object* stack,
                                    SMOP__ResponderInterface* self,
                                    SMOP__Object* identifier,
                                    SMOP__Object* capture) {
  if (identifier == method1) {
    printf("ok 1 - method1 called.\n");
  } else if (identifier == method2) {
    printf("ok 2 - method1 called.\n");
  } else if (identifier == method3) {
    printf("ok 3 - method1 called.\n");
  } else if (identifier == method4) {
    printf("ok 4 - method1 called.\n");
  } else if (identifier == DESTROYALL) {
    printf("ok 5 - method1 called.\n");
  } else {
    printf("not ok - unknown method called %p.\n",identifier);
  }
}

int main(int argc, char** argv) {
  printf("1..5\n");

  smop_init();
  SMOP__Object* stack = SMOP_DISPATCH(NULL, SMOP_RI(SMOP__STACK__Stack),
                                      SMOP__STACK__Stack_new, NULL);

  SMOP__Object* obj = smop_lowlevel_alloc(sizeof(SMOP__ResponderInterface));
  SMOP__ResponderInterface* ri = (SMOP__ResponderInterface*)obj;
  ri->MESSAGE = custom_MESSAGE;
  ri->REFERENCE = smop_lowlevel_refcnt_inc;
  ri->RELEASE = smop_lowlevel_refcnt_dec;


  SMOP__Object* node;

  /*#!# sm0p code
   *

  node = q:sm0p {
     ;
     obj.method1();
     obj.method2();
     obj.method3();
     obj.method4();
  };

   *
   */

  SMOP__Object* push_capture = smop__stack__stack_push_capture(stack, node);
  SMOP_DISPATCH(NULL, SMOP_RI(stack),
                SMOP__STACK__Stack_push, push_capture);
  SMOP_RELEASE(NULL,push_capture);


  SMOP__Object* loop_capture = smop__stack__stack_loop_capture(stack);
  SMOP_DISPATCH(NULL, SMOP_RI(stack),
                SMOP__STACK__Stack_loop, loop_capture);
  SMOP_RELEASE(NULL,loop_capture);

  SMOP_RELEASE(NULL,obj);
  SMOP_RELEASE(NULL,node);
  SMOP_RELEASE(NULL,stack);

  smop_destr();

  return 0;
}
