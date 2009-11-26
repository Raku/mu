#include <stdlib.h>
#include <stdio.h>
#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/interpreter.h>
#include <smop/capture.h>

/* In this test, we just want to see if we can create an nagc object
   and release it, seeing if it gets destroyed accordingly. */
static void custom_destroyall(SMOP__Object* interpreter,
                              SMOP__Object* value) {
  printf("ok 4 # continuation destroyed\n");
}

static SMOP__Object* custom_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  static int counter=0;
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  if (identifier == SMOP__NATIVE__idconst_createn("eval",4)) {
    counter++;
    if (counter == 1) {
      printf("ok 2 # eval called\n");
      ret = SMOP__NATIVE__bool_true;
    } else if (counter == 2) {
      printf("ok 3 # eval called second time\n");
    } else {
      printf("not ok 3 # eval called third time\n");
    }
  }  else {
    printf("not ok # wrong method called\n");
  }  
  SMOP_RELEASE(interpreter,capture);
  return ret;
}

int main() {

  printf("1..4\n");

  smop_s0native_init();
  smop_nagc_init();
  smop_capture_init();
  smop_interpreter_init();

  SMOP__NAGC__ResponderInterface ri;
  ri.RI = NULL;
  ri.MESSAGE = custom_message;
  ri.REFERENCE = smop_nagc_reference;
  ri.RELEASE = smop_nagc_release;
  ri.WEAKREF = smop_nagc_weakref;
  ri.id = "test ri";
  ri.DESTROYALL = custom_destroyall;

  SMOP__Object* obj = smop_nagc_alloc(sizeof(SMOP__NAGC__Object));
  obj->RI = (SMOP__ResponderInterface*)&ri;

  SMOP__Object* interpreter = SMOP_interpreter_create(SMOP__EmptyInterpreter);
  printf("ok 1 # lives after interpreter creation\n");


  SMOP_DISPATCH(SMOP__EmptyInterpreter, SMOP_RI(interpreter), SMOP__NATIVE__idconst_createn("goto",4),SMOP__NATIVE__capture_create(interpreter,(SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter), obj, NULL}, (SMOP__Object*[]) {NULL}));

  SMOP_DISPATCH(SMOP__EmptyInterpreter, SMOP_RI(interpreter), SMOP__NATIVE__idconst_createn("loop",4),SMOP__NATIVE__capture_create(interpreter,(SMOP__Object*[]) {interpreter, NULL}, (SMOP__Object*[]) {NULL}));


  smop_interpreter_destr();
  smop_capture_destr();
  smop_nagc_destr();
  smop_s0native_destr();

  return 0;
}
