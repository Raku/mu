#ifndef SMOP_P5_H
#include <EXTERN.h>
#include <perl.h>
#define SMOP_P5_H
extern SMOP__Object* SMOP__P5Interpreter;
SMOP__Object* SMOP__P5__SV_create(SMOP__Object* interpreter,SMOP__Object* p5interpreter,SV* sv);
PerlInterpreter* SMOP__P5__p5interpreter_unbox(SMOP__Object* interpreter,SMOP__Object* p5interpreter);
#endif
