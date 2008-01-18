
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
    printf("ok 2 - method2 called.\n");
  } else if (identifier == method3) {
    printf("ok 3 - method3 called.\n");
  } else if (identifier == method4) {
    printf("ok 4 - method4 called.\n");
  } else if (identifier == DESTROYALL) {
    printf("ok 5 - DESTROYALL called.\n");
  } else {
    printf("not ok - unknown method called %p.\n",identifier);
  }
}

int main(int argc, char** argv) {
  printf("1..5\n");

  smop_init();

  SMOP__Object* intrp = SMOP_DISPATCH(NULL, SMOP_RI(SMOP__INTPTR__InterpreterInstance),
                                      SMOP__ID__new, smop__intptr__invocant_capture_new
                                      (SMOP__INTPTR__InterpreterInstance,NULL));

  SMOP__Object* obj = smop_lowlevel_alloc(sizeof(SMOP__ResponderInterface));
  SMOP__ResponderInterface* ri = (SMOP__ResponderInterface*)obj;
  ri->MESSAGE = custom_MESSAGE;
  ri->REFERENCE = smop_lowlevel_refcnt_inc;
  ri->RELEASE = smop_lowlevel_refcnt_dec;


  SMOP__Object* node;

  node = q:sm0p {
    ; # keep an empty node at the start, as this node will not be evaluated.
    $obj.method1();
    $obj.method2();
    $obj.method3();
    $obj.method4();
  };
  
  SMOP__Object* goto_capture = smop__intptr__goto_capture_new(intrp, node);
  SMOP_DISPATCH(NULL, SMOP_RI(intrp),
                SMOP__ID__goto, goto_capture);
  SMOP_RELEASE(NULL,goto_capture);


  SMOP__Object* loop_capture = smop__intptr__invocant_capture_new(intrp);
  SMOP_DISPATCH(NULL, SMOP_RI(intrp),
                SMOP__ID__loop, loop_capture);
  SMOP_RELEASE(NULL, loop_capture);

  SMOP_RELEASE(NULL,obj);
  SMOP_RELEASE(NULL,node);
  SMOP_RELEASE(NULL,intrp);

  smop_destr();

  return 0;
}
