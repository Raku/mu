#ifndef SMOP_INTERPRETER_H
#define SMOP_INTERPRETER_H

#include <smop/base.h>
SMOP__Object* SMOP_interpreter_create(SMOP__Object* interpreter);
void smop_interpreter_init(void);
void smop_interpreter_destr(void);

SMOP__Object* smop_shortcut_interpreter_goto(SMOP__Object* interpreter,SMOP__Object* continuation);
#endif
