#include <stdio.h>
#include <smop.h>
#include <smop_s1p.h>
#include <smop_mold.h>
#include <smop_lowlevel.h>



void coro(void *data) {
    printf("ok 2\n");
    SMOP__Object* interpreter = data;
    SMOP__Object* continuation = SMOP_DISPATCH(interpreter,SMOP_RI(interpreter),SMOP__NATIVE__idconst_create("continuation"),SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,interpreter),NULL,NULL));
    SMOP_RELEASE(interpreter,SMOP_DISPATCH(interpreter,SMOP_RI(continuation),SMOP__NATIVE__idconst_create("finished"),SMOP__NATIVE__capture_create(interpreter,continuation,NULL,NULL)));
}
int main(int argc, char** argv) {
  smop_init();

  SMOP__Object* interpreter = SMOP_DISPATCH(SMOP__INTPTR__InterpreterInstance, SMOP_RI(SMOP__INTPTR__InterpreterInstance),
                                            SMOP__ID__new, 
                                            SMOP__NATIVE__capture_create(SMOP__INTPTR__InterpreterInstance,
                                                                         SMOP__INTPTR__InterpreterInstance,NULL,NULL));

  printf("1..2\n");
  SMOP__Object* pcl = SMOP__S1P__PCLCoro_create(interpreter,coro,interpreter,NULL,8192);
  printf("ok 1 - lives after PCL coro creation\n");

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                SMOP__ID__goto,
                pcl);

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
   SMOP__ID__loop, SMOP__NATIVE__capture_create(interpreter,
     SMOP_REFERENCE(interpreter,interpreter),
     NULL, NULL));

  /*SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
   SMOP__ID__loop, SMOP__NATIVE__capture_create(interpreter,
     SMOP_REFERENCE(interpreter,interpreter),
     NULL, NULL));*/


  SMOP_RELEASE(SMOP__INTPTR__InterpreterInstance,interpreter);

  smop_destr();

  return 0;
}
