#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/interpreter.h>
#include <smop/capture.h>
#include <smop/native.h>
#include <stdio.h>

int main(int argc, char** argv) {
  printf("1..2\n");

  smop_s0native_init();
  smop_nagc_init();
  smop_capture_init();
  smop_interpreter_init();


  // For starts, there will be some functions to
  // wrap the dispatch, after the list is built,
  // we can start to use the dispatcher itself
  SMOP__Object* interpreter = SMOP_interpreter_create(SMOP__EmptyInterpreter);

  smop_native_init(interpreter);

  SMOP__Object* myint = SMOP__NATIVE__int_create(1234);
  if (!myint) printf("not ");
  printf("ok 1 - create works...\n");
  
  if (SMOP__NATIVE__int_fetch(myint) != 1234) printf("not ");
  printf("ok 2 - fetch works...\n");



  SMOP_RELEASE(interpreter, myint);

  smop_native_destr(interpreter);
  smop_capture_destr();
  smop_nagc_destr();
  smop_interpreter_destr();
  smop_s0native_destr();
  return 0;
}
