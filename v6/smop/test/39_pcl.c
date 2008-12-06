#include <stdio.h>
#include <smop.h>
#include <smop_s1p.h>
#include <smop_mold.h>
#include <smop_lowlevel.h>
#include <smop_pcl.h>



void coro(void *data) {
    SMOP__Object* interpreter = data;
    printf("ok 2 - calling a coro works\n");
    co_resume();
    printf("ok 3 - control returns after co_resume\n");
    SMOP__Object* continuation = SMOP_DISPATCH(interpreter,SMOP_RI(interpreter),SMOP__NATIVE__idconst_create("continuation"),SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,interpreter),NULL,NULL));
  SMOP__Object* mold = SMOP__Mold_create(2,
    (SMOP__Object*[]) { /* constants */
      SMOP__S1P__IO_create(interpreter), 
      SMOP__NATIVE__idconst_create("print"), 
      SMOP__S1P__Str_create("ok 4 - switching to a mold frame works\n"), 
      SMOP_REFERENCE(interpreter,interpreter),
      SMOP__NATIVE__idconst_create("goto"), 
      SMOP_REFERENCE(interpreter,continuation),
      NULL
    },15,(int[]) {
    1,6,0,1,1,2,0, //$r6 = $r0.$r1($r2)
    1,7,3,4,1,5,0, //$r7 = $r3.$r4($r5)
    0
  });
  SMOP__Object* frame = SMOP__Mold__Frame_create(interpreter,mold);

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                SMOP__ID__goto,
                frame);

  co_resume();

  printf("ok 5 - switching back to pcl coro works\n");

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
