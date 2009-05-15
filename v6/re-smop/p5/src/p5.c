#include <smop/base.h>
#include <smop/s0native.h>
#include <stdio.h>
void smop_p5_init(SMOP__Object* interpreter) {
  smop_p5interpreter_init(interpreter);
  smop_p5_sv_init(interpreter);
  smop_p5_coro_init(interpreter);
}
void smop_p5_destr(SMOP__Object* interpreter) {
  smop_p5_coro_destr(interpreter);
  smop_p5_sv_destr(interpreter);
  smop_p5interpreter_destr(interpreter);
}
