#include <stdlib.h>
#include <stdio.h>
#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/interpreter.h>
#include <smop/capture.h>
#include <smop/mold.h>

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
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  if (identifier == SMOP__NATIVE__idconst_create("ok2")) {
    printf("ok 2\n");
  } else if (identifier == SMOP__NATIVE__idconst_create("print")) {
    SMOP__Object* pos1 = SMOP__NATIVE__capture_positional(interpreter,capture,1);
    int len;
    char* s = SMOP__NATIVE__idconst_fetch_with_null(pos1,&len);
    printf("%s\n",s);
    free(s);
  } else {
    printf("not ok\n");
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
  smop_mold_init();

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

  SMOP__Object* mold = SMOP__Mold_create(1,
    (SMOP__Object*[]) { /* constants */
      obj,
      SMOP__NATIVE__idconst_create("ok2"), 
      SMOP__NATIVE__idconst_create("print"), 
      SMOP__NATIVE__idconst_create("ok 3"), 
      NULL
    },14,(int[]) {
    1,4,0,1,0,0, //$r4 = $r0.$r1()
    1,4,0,2,1,3,0, //$r4 = $r0.$r2($r3) 
    0 
  });

  SMOP__Object* frame = SMOP__Mold__Frame_create(interpreter,mold);

  printf("ok 1 # lives after frame creation\n");

  SMOP_DISPATCH(SMOP__EmptyInterpreter, SMOP_RI(interpreter), SMOP__NATIVE__idconst_createn("goto",4),SMOP__NATIVE__capture_create(interpreter,(SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter), frame, NULL}, (SMOP__Object*[]) {NULL}));

  SMOP_DISPATCH(SMOP__EmptyInterpreter, SMOP_RI(interpreter), SMOP__NATIVE__idconst_createn("loop",4),SMOP__NATIVE__capture_create(interpreter,(SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter), NULL}, (SMOP__Object*[]) {NULL}));

  SMOP_RELEASE(interpreter,interpreter);


  smop_interpreter_destr();
  smop_mold_destr();
  smop_capture_destr();
  smop_nagc_destr();
  smop_s0native_destr();

  return 0;
}
