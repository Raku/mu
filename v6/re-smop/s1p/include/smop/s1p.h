
#ifndef SMOP_S1P_H
#define SMOP_S1P_H

#include <smop/base.h>
extern SMOP__Object* SMOP__S1P__LexicalPrelude;
void smop_s1p_init(SMOP__Object* interpreter);
void smop_s1p_destr(SMOP__Object* interpreter);
SMOP__Object* SMOP__S1P__Scalar_create(SMOP__Object* interpreter,SMOP__Object* initial_value);

#endif
