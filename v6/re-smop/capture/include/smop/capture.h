#ifndef SMOP_CAPTURE_H
#include <smop/base.h>
#define SMOP_CAPTURE_H
SMOP__Object* SMOP__NATIVE__capture_create(SMOP__Object* interpreter,SMOP__Object**  positional,SMOP__Object **named);
SMOP__Object* SMOP__NATIVE__capture_positional(SMOP__Object* interpreter,SMOP__Object* capture,int i);
void smop_capture_init(void);
void smop_capture_destr(void);
#endif


