#include <stdlib.h>
#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/util.h>
#include <smop/capture.h>
#include <smop/nagc.h>
#include <stdio.h>

static SMOP__Object* SMOP__RI;
static SMOP__Object* SMOP__ID__FETCH;

static SMOP__Object* message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {

  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  SMOP__Object* invocant = (SMOP__Object*) SMOP__NATIVE__capture_positional(interpreter, capture,0);

  if (identifier == SMOP__ID__FETCH) {
    ___VALUE_FETCH___;
  } else {
    ___UNKNOWN_METHOD___;
  }

  SMOP_RELEASE(interpreter,invocant);
  SMOP_RELEASE(interpreter,capture);
  return ret;
}


void smop_nagc_ri_init() {
  SMOP__RI = malloc(sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__RI)->MESSAGE = message;
  ((SMOP__ResponderInterface*)SMOP__RI)->REFERENCE = smop_noop_reference;
  ((SMOP__ResponderInterface*)SMOP__RI)->RELEASE = smop_noop_release;
  ((SMOP__ResponderInterface*)SMOP__RI)->id = "meta RI";
  SMOP__RI->RI = (SMOP__ResponderInterface*)SMOP__RI;
  SMOP__ID__FETCH = SMOP__NATIVE__idconst_create("FETCH");
}

void smop_nagc_ri_destr() {
  free(SMOP__RI);
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
  void (*DESTROYALL)  (SMOP__Object* interpreter,SMOP__Object* object),
  char *id
  ) {
    
    SMOP__NAGC__ResponderInterface* ri = (SMOP__NAGC__ResponderInterface*) malloc(sizeof(SMOP__NAGC__ResponderInterface));;
    ri->RI = (SMOP__ResponderInterface*)SMOP__RI;
    ri->MESSAGE = MESSAGE;
    ri->REFERENCE = REFERENCE;
    ri->RELEASE = RELEASE;
    ri->WEAKREF = WEAKREF;
    ri->DESTROYALL = DESTROYALL;
    ri->id = id;
    return (SMOP__Object*)ri;
}
