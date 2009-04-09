
#ifndef SMOP_S1P_H
#define SMOP_S1P_H

#include <smop/base.h>
extern SMOP__Object* SMOP__S1P__LexicalScope;
extern SMOP__Object* SMOP__S1P__LexicalPrelude;
void smop_s1p_init(SMOP__Object* interpreter);
void smop_s1p_destr(SMOP__Object* interpreter);
void smop_s1p_lexicalscope_init(SMOP__Object* interpreter);
void smop_s1p_lexicalscope_destr(SMOP__Object* interpreter);
void smop_s1p_lexical_prelude_init(SMOP__Object* interpreter);
void smop_s1p_lexical_prelude_destr(SMOP__Object* interpreter);

void smop_s1p_proto_init(SMOP__Object* interpreter);
void smop_s1p_proto_destr(SMOP__Object* interpreter);
void smop_s1p_scalar_destr(SMOP__Object* interpreter);
void smop_s1p_scalar_init(SMOP__Object* interpreter);

void smop_s1p_hash_init(SMOP__Object* interpreter);
void smop_s1p_hash_destr(SMOP__Object* interpreter);

void smop_s1p_hash_bvalue_init(SMOP__Object* interpreter);
void smop_s1p_hash_bvalue_destr(SMOP__Object* interpreter);

SMOP__Object* SMOP__S1P__Scalar_create(SMOP__Object* interpreter,SMOP__Object* initial_value);

SMOP__Object* SMOP__Proto__create(SMOP__Object* delegate_to_RI);
SMOP__Object* SMOP__S1P__Hash_create(SMOP__Object* interpreter);
SMOP__Object* SMOP__S1P__Hash_BValue_create(SMOP__Object* interpreter, SMOP__Object* owner, SMOP__Object* key);
#endif
