#include <stdlib.h>
#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/util.h>
#include <smop/nagc.h>
#include <smop/dump.h>
#include <stdio.h>

static SMOP__Object* RI;


void smop_nagc_ri_init() {
  RI = malloc(sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)RI)->MESSAGE = smop_placeholder_message;
  ((SMOP__ResponderInterface*)RI)->REFERENCE = smop_noop_reference;
  ((SMOP__ResponderInterface*)RI)->RELEASE = smop_noop_release;
  ((SMOP__ResponderInterface*)RI)->DUMP = smop_ri_dump;
  ((SMOP__ResponderInterface*)RI)->id = "meta RI";
  RI->RI = (SMOP__ResponderInterface*)RI;
}

void smop_nagc_ri_destr() {
  free(RI);
}

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
  SMOP__Object* (*WEAKREF)  (SMOP__Object* interpreter,           
                             SMOP__ResponderInterface* self,      
                             SMOP__Object* object),
  SMOP__Object* (*DUMP)  (SMOP__Object* interpreter,           
                             SMOP__ResponderInterface* self,      
                             SMOP__Object* object),
  void (*DESTROYALL)  (SMOP__Object* interpreter,SMOP__Object* object),
  char *id
  ) {
    
    SMOP__NAGC__ResponderInterface* ri = (SMOP__NAGC__ResponderInterface*) malloc(sizeof(SMOP__NAGC__ResponderInterface));;
    ri->RI = (SMOP__ResponderInterface*)RI;
    ri->MESSAGE = MESSAGE;
    ri->REFERENCE = REFERENCE;
    ri->RELEASE = RELEASE;
    ri->WEAKREF = WEAKREF;
    ri->DESTROYALL = DESTROYALL;
    ri->DUMP = DUMP;
    ri->id = id;
    return (SMOP__Object*)ri;
}
