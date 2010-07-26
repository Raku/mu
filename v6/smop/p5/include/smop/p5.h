#ifndef SMOP_P5_H
#include <smop/base.h>
#include <EXTERN.h>
#include <perl.h>
#define SMOP_P5_H
extern SMOP__Object* SMOP__P5Interpreter;

extern SMOP__Object* SMOP__P5__smop_interpreter;
extern SV* SMOP__P5__current_coro_state;
extern SMOP__Object* SMOP__P5__current_back;
extern SMOP__Object* SMOP__P5__smop_p5interpreter;
extern SV* SMOP__P5__result_sv;
extern int* SMOP__P5__current_coro_has_next;

SMOP__Object* SMOP__P5__SV_create(SMOP__Object* interpreter,SMOP__Object* p5interpreter,SV* sv);
PerlInterpreter* SMOP__P5__p5interpreter_unbox(SMOP__Object* interpreter,SMOP__Object* p5interpreter);
SMOP__Object* SMOP__P5__Coro_create(SMOP__Object* interpreter,SMOP__Object* p5interpreter,SV* coro);
SV* SMOP__Object2SV(SMOP__Object* interpreter,pTHX_ SMOP__Object* object);
void SMOP__P5__transfer_to_main_coro(pTHX_ SMOP__Object* interpreter);

void smop_p5interpreter_init(SMOP__Object* interpreter);
void smop_p5interpreter_destr(SMOP__Object* interpreter);

void smop_p5_sv_init(SMOP__Object* interpreter);
void smop_p5_sv_destr(SMOP__Object* interpreter);

void smop_p5_coro_init(SMOP__Object* interpreter);
void smop_p5_coro_destr(SMOP__Object* interpreter);

void smop_p5_init(SMOP__Object* interpreter);
void smop_p5_destr(SMOP__Object* interpreter);
#endif
