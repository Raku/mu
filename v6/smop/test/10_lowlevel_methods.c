#include <stdio.h>
#include <pthread.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>
#include <unistd.h>


SMOP__Object* test_code(SMOP__Object* interpreter,
                        SMOP__Object* method,
                        SMOP__Object* responder,
                        SMOP__Object* identifier,
                        SMOP__Object* capture) {
  printf("ok 2 - method call.\n");
  SMOP_RELEASE(interpreter,responder);
  SMOP_RELEASE(interpreter,identifier);
  SMOP_RELEASE(interpreter,capture);
  return SMOP__NATIVE__bool_false;
}


int main(int argc, char** argv) {
  printf("1..4\n");
  smop_init();

  SMOP__Object* intrp = SMOP_DISPATCH(SMOP__INTPTR__InterpreterInstance, SMOP_RI(SMOP__INTPTR__InterpreterInstance),
                                      SMOP__ID__new, 
                                      SMOP__NATIVE__capture_create(SMOP__INTPTR__InterpreterInstance,
                                                                   SMOP__INTPTR__InterpreterInstance,NULL,NULL));

  SMOP__Object* method = SMOP__S1P__Method_create(0,SMOP__ID__new,
                                                  SMOP__NATIVE__bool_false,
                                                  test_code);
  printf("ok 1 - method created.\n");

  SMOP_DISPATCH(intrp,
                SMOP_RI(method),
                SMOP__ID__call,
                SMOP__NATIVE__capture_create(intrp,
                                             SMOP_REFERENCE(intrp,method),
                                             (SMOP__Object*[]){ SMOP__NATIVE__bool_true,
                                                                SMOP__NATIVE__bool_true,
                                                                SMOP__NATIVE__bool_true, NULL}, NULL));


  SMOP_RELEASE(intrp, method);

  SMOP_DISPATCH(intrp, SMOP_RI(intrp),
                SMOP__ID__loop, SMOP__NATIVE__capture_create(intrp,
                                                             SMOP_REFERENCE(intrp,intrp),
                                                             NULL, NULL));

  printf("ok 3 - returned.\n");

  SMOP_RELEASE(SMOP__INTPTR__InterpreterInstance,intrp);

  smop_destr();

  printf("ok 4 - destroyed.\n");

  return 0;
  
}
