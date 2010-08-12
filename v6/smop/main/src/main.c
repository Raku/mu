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

void smop_p5_init(SMOP__Object* interpreter);
void smop_p5_destr(SMOP__Object* interpreter);

static SMOP__Object* interpreter;
SMOP__Object* smop_main_get_interpreter() {
  if (!interpreter) {
    smop_s0native_init();
    smop_dump_init();
    smop_nagc_init();
    smop_capture_init();
    smop_interpreter_init();
    smop_mold_init();
    smop_yeast_init();
  
    interpreter = SMOP_interpreter_create(SMOP__EmptyInterpreter);
  
    smop_native_init(interpreter);
    smop_s1p_init(interpreter);
  
    smop_lost_init(interpreter);
    smop_p6opaque_init(interpreter);
    smop_s1p_oo_init(interpreter);
  
    smop_p5_init(interpreter);
  
    smop_mold_message_init(interpreter);
  }
  return interpreter;
}

