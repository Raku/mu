
#ifndef SMOP_NATIVE_H
#define SMOP_NATIVE_H

#include <smop/base.h>
int SMOP__NATIVE__int_fetch(SMOP__Object* value);
SMOP__Object* SMOP__NATIVE__int_create(int value);


void smop_native_init(SMOP__Object* interpreter);
void smop_native_destr(SMOP__Object* interpreter);
void smop_native_int_init(SMOP__Object* interpreter);
void smop_native_int_destr(SMOP__Object* interpreter);
void smop_nagc_ri_init();
void smop_nagc_ri_destr();
void smop_capture_message_init(SMOP__Object* interpreter);
void smop_capture_message_destr(SMOP__Object* interpreter);
void smop_bool_message_init(SMOP__Object* interpreter);
void smop_bool_message_destr(SMOP__Object* interpreter);
void smop_idconst_message_init(SMOP__Object* interpreter); 
void smop_idconst_message_destr(SMOP__Object* interpreter); 
#endif
