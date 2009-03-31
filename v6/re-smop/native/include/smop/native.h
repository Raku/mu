
#ifndef SMOP_NATIVE_H
#define SMOP_NATIVE_H

#include <smop/base.h>
int SMOP__NATIVE__int_fetch(SMOP__Object* value);
SMOP__Object* SMOP__NATIVE__int_create(int value);

SMOP__Object* SMOP__NAGC__RI__create(
  SMOP__Object* (*MESSAGE)  (SMOP__Object* interpreter,           
                             SMOP__ResponderInterface* self,      
                             SMOP__Object* identifier,            
                             SMOP__Object* capture),              
  SMOP__Object* (*REFERENCE)(SMOP__Object* interpreter,           
                             SMOP__ResponderInterface* self,      
                             SMOP__Object* object),               
  SMOP__Object* (*RELEASE)  (SMOP__Object* interpreter,           
                             SMOP__ResponderInterface* self,      
                             SMOP__Object* object),
  void (*DESTROYALL)  (SMOP__Object* interpreter,SMOP__Object* object),
  char *id
);

void smop_native_init(SMOP__Object* interpreter);
void smop_native_destr(SMOP__Object* interpreter);
void smop_native_int_init(SMOP__Object* interpreter);
void smop_native_int_destr(SMOP__Object* interpreter);
void smop_nagc_ri_init();
void smop_nagc_ri_destr();

#endif
