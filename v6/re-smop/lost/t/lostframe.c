
#include <stdlib.h>
#include <stdio.h>
#include <smop/base.h>
#include <smop/nagc.h>
#include <smop/lost.h>
#include <smop/interpreter.h>
#include <smop/s0native.h>
#include <smop/capture.h>
#include <smop/native.h>
#include <string.h>

/*
#include <smop/nagc.h>
#include <smop/capture.h>
#include <smop/native.h>
#include <smop/mold.h>
#include <stdio.h>*/

static int step(SMOP__Object* interpreter,
                SMOP__Object* obj) {

  /* TODO locking */
  SMOP_LOST_Frame* frame = (SMOP_LOST_Frame*) obj;

  switch (frame->pc) {
  case 0:
    printf("ok 2 - first execution is in 0\n");
    break;
  case 1:
    printf("ok 3 - got back from first execution\n");
    break;
  case 2:
    if (strcmp((char*) frame->user,"test")) {
      printf("not ");
    }
    printf("ok 4 - the user data is correct\n");
    break;
  case 3:
    frame->pc++;
    break;
  case 4:
    printf("not ");
    break;
  case 5:
    printf("ok 5 - user can change the pc\n");
    break;
  case 6:
    printf("ok 6 - let's exit\n");
    return 0;
  case 7:
    printf("not ok 7 - it shouldn't get here\n");
    return 0;
  };
  // return true while it doesn't reach the end
  return 1;
}


static int destr(SMOP__Object* interpreter,
                 SMOP__Object* frame) {
  printf("ok 7 - destroyed\n");
}

int main() {

  printf("1..7\n");

  smop_s0native_init();
  smop_nagc_init();
  smop_capture_init();
  smop_interpreter_init();

  char* foo = "test";

  SMOP__Object* interpreter = SMOP_interpreter_create(SMOP__EmptyInterpreter);

  smop_native_init(interpreter);
  smop_lost_init();

  SMOP__Object* frame = SMOP__LOST__Frame_create(interpreter,SMOP__NATIVE__bool_false,foo,step,destr);
  printf("ok 1 # lives after frame creation\n");


  /*
   * Now we're going to execute this frame...
   */
  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter), SMOP__NATIVE__idconst_create("goto"),SMOP__NATIVE__capture_create(interpreter,(SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter), frame, NULL}, (SMOP__Object*[]) {NULL}));
  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter), SMOP__NATIVE__idconst_create("loop"),SMOP__NATIVE__capture_create(interpreter,(SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter), NULL}, (SMOP__Object*[]) {NULL}));
  SMOP_RELEASE(interpreter,interpreter);


  smop_lost_destr();
  smop_interpreter_destr();
  smop_capture_destr();
  smop_nagc_destr();
  smop_s0native_destr();

  return 0;
}
