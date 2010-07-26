package SMOP::Boilerplate;
our $BOILERPLATE = <<'END';
#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/interpreter.h>
#include <smop/capture.h>
#include <smop/native.h>
#include <smop/mold.h>
#include <smop/s1p.h>
#include <smop/lost.h>
#include <smop/p6opaque.h>
#include <smop/s1p-oo.h>
#include <smop/yeast.h>
#include <smop/profile.h>
#include <smop/dump.h>
#include <smop/nagc.h>
#include <stdio.h>

/*void smop_p5_init(SMOP__Object* interpreter);
void smop_p5_destr(SMOP__Object* interpreter);*/

/* Your helper function go here */
%%FUNCS%%

int main(int argc, char** argv) {
  smop_s0native_init();
  smop_dump_init();
  smop_nagc_init();
  smop_capture_init();
  smop_interpreter_init();
  smop_mold_init();
  smop_yeast_init();

  SMOP__Object* interpreter = SMOP_interpreter_create(SMOP__EmptyInterpreter);

  smop_native_init(interpreter);
  smop_s1p_init(interpreter);

  smop_lost_init(interpreter);
  smop_p6opaque_init(interpreter);
  smop_s1p_oo_init(interpreter);

  smop_p5_init(interpreter);

  smop_mold_message_init(interpreter);


  /* The frame creation code goes here */
  %%BODY%%

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
    SMOP__NATIVE__idconst_create("goto"),
    SMOP__NATIVE__capture_create(interpreter,
        (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),frame,NULL},
        (SMOP__Object*[]) {NULL})
  );

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                SMOP__NATIVE__idconst_create("loop"),
                SMOP__NATIVE__capture_create(
                    interpreter,
                    (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),NULL}
                    ,(SMOP__Object*[]) {NULL}));

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
    SMOP__NATIVE__idconst_create("goto"),
    SMOP__NATIVE__capture_create(
        interpreter,
        (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),SMOP__NATIVE__bool_false,NULL}
        ,(SMOP__Object*[]) {NULL}));

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                SMOP__NATIVE__idconst_create("loop") , SMOP__NATIVE__capture_create(
                    interpreter,
                    (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),NULL},
                    (SMOP__Object*[]) {NULL}));



  smop_mold_message_destr(interpreter);
  smop_p5_destr(interpreter);
  smop_s1p_oo_destr(interpreter);
  smop_p6opaque_destr(interpreter);
  smop_lost_destr(interpreter);
  smop_s1p_destr(interpreter);
  smop_native_destr(interpreter);

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                SMOP__NATIVE__idconst_create("loop"),
                SMOP__NATIVE__capture_create(
                    interpreter,
                    (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),NULL}
                    ,(SMOP__Object*[]) {NULL}));

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
    SMOP__NATIVE__idconst_create("goto"),
    SMOP__NATIVE__capture_create(
        interpreter,
        (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),SMOP__NATIVE__bool_false,NULL}
        ,(SMOP__Object*[]) {NULL}));

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                SMOP__NATIVE__idconst_create("loop") , SMOP__NATIVE__capture_create(
                    interpreter,
                    (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),NULL},
                    (SMOP__Object*[]) {NULL}));

  SMOP_RELEASE(SMOP__EmptyInterpreter,interpreter);


  smop_yeast_destr();
  smop_mold_destr();
  smop_interpreter_destr();
  smop_capture_destr();
  smop_nagc_destr();
  smop_dump_destr();
  smop_s0native_destr();
  smop_s1p_close_dlhandles();

  return 0;
}
END
1;
