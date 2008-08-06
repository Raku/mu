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

  printf("1..3\n");
  SMOP__Object* mold = SMOP__Mold_create(1,
    (SMOP__Object*[]) { /* constants */
      SMOP__S1P__IO_create(), //$r4
      SMOP__NATIVE__idconst_create("print"), //$r5
      SMOP__S1P__Str_create("ok 2 - simple method call works\n"), //$r5
      NULL
    },8,(int[]) {
    1,7,4,5,1,6,0,0 //$r7 = $r4.$r5($r6)
  });
  printf("ok 1 - lives after Mold creation\n");
  SMOP__Object* frame = SMOP__Mold__Frame_create(interpreter,mold);

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                SMOP__ID__goto,
                frame);

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
   SMOP__ID__loop, SMOP__NATIVE__capture_create(interpreter,
     SMOP_REFERENCE(interpreter,interpreter),
     NULL, NULL));

  SMOP__Object* mold2 = SMOP__Mold_create(1,
    (SMOP__Object*[]) {
      SMOP__S1P__RootNamespace,
      SMOP__NATIVE__idconst_create("postcircumfix:{ }"),
      SMOP__NATIVE__idconst_create("$*OUT"),
      SMOP__NATIVE__idconst_create("print"),
      SMOP__S1P__Str_create("ok 3\n"),
      SMOP__NATIVE__idconst_create("FETCH"),
      NULL
    },21,(int[]) {
    1,10,4,5,1,6,0,  //$r9 = $r4.$r5($r6)
    1,11,10,9,0,0,
    1,12,11,7,1,8,0, //$r10 = $r9.r7($r8)
    0
  });
  SMOP__Object* frame2 = SMOP__Mold__Frame_create(interpreter,mold2);

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                SMOP__ID__goto,
                frame2);

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
   SMOP__ID__loop, SMOP__NATIVE__capture_create(interpreter,
     SMOP_REFERENCE(interpreter,interpreter),
     NULL, NULL));


  SMOP_RELEASE(SMOP__INTPTR__InterpreterInstance,interpreter);

  smop_destr();

  return 0;
}
