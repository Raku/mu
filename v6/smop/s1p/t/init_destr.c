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
#include <stdio.h>

void smop_p5_init(SMOP__Object* interpreter);
void smop_p5_destr(SMOP__Object* interpreter);

int main(int argc, char** argv) {
  smop_s0native_init();
  smop_nagc_init();
  smop_capture_init();
  smop_interpreter_init();
  smop_yeast_init();

  SMOP__Object* interpreter = SMOP_interpreter_create(SMOP__EmptyInterpreter);

  smop_native_init(interpreter);
  smop_s1p_init(interpreter);

  printf("1..1\nok 1 - memory test only... look for valgrind output...\n");

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

  smop_s1p_destr(interpreter);

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

  smop_native_destr(interpreter);

  SMOP_RELEASE(SMOP__EmptyInterpreter,interpreter);


  smop_mold_destr();
  smop_interpreter_destr();
  smop_capture_destr();
  smop_nagc_destr();
  smop_s0native_destr();

  return 0;
}
