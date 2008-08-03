#include <stdio.h>
#include <smop.h>
#include <smop_s1p.h>
#include <smop_mold.h>
#include <smop_lowlevel.h>



int main(int argc, char** argv) {
  smop_init();

  SMOP__Object* interpreter = SMOP_DISPATCH(SMOP__INTPTR__InterpreterInstance, SMOP_RI(SMOP__INTPTR__InterpreterInstance),
                                            SMOP__ID__new, 
                                            SMOP__NATIVE__capture_create(SMOP__INTPTR__InterpreterInstance,
                                                                         SMOP__INTPTR__InterpreterInstance,NULL,NULL));

  printf("1..2\n");
  SMOP__Object* mold = SMOP__Mold_create(1,
    (SMOP__Object*[]) { /* constant */
      SMOP__S1P__IO_create(), //$r4
      SMOP__NATIVE__idconst_create("print"), //$r5
      SMOP__S1P__Str_create("ok 2 - simple method call works\n"), //$r5
      NULL
    },8,(int[]) {
    1,4,5,1,6,0,7,0 //$r7 = $r4.$r5($r6)
  });
  printf("ok 1 - lives after Mold creation\n");
  SMOP__Object* frame = SMOP__Mold__Frame_create(mold);

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                SMOP__ID__goto,
                frame);

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
   SMOP__ID__loop, SMOP__NATIVE__capture_create(interpreter,
     SMOP_REFERENCE(interpreter,interpreter),
     NULL, NULL));


  SMOP_RELEASE(SMOP__INTPTR__InterpreterInstance,interpreter);

  smop_destr();

  return 0;
}
