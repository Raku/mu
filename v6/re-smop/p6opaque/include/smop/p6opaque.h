#ifndef SMOP_P6OPAQUE_H
#define SMOP_P6OPAQUE_H

#include <smop/base.h>
SMOP__Object* smop_p6opaque_proto_storage_create(SMOP__Object* interpreter);

void smop_p6opaque_proto_init(SMOP__Object* interpreter);
void smop_p6opaque_proto_destr(SMOP__Object* interpreter);

void smop_p6opaque_init(SMOP__Object* interpreter);
void smop_p6opaque_destr(SMOP__Object* interpreter);

void smop_p6opaque_default_how_init(SMOP__Object* interpreter);
void smop_p6opaque_default_how_destr(SMOP__Object* interpreter);

#endif
