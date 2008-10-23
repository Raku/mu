#include <stdlib.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>
#include <stdio.h>

SMOP__Object* SMOP__RI;

static SMOP__Object* message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {

  ___CONST_IDENTIFIER_ONLY___;
  ___NATIVE_CAPTURE_ONLY___;
  ___INVOCANT_RI_SHOULD_MATCH___;

  SMOP__Object* ret = SMOP__NATIVE__bool_false;

  if (identifier == SMOP__ID__DESTROYALL) {
  } else {
    ___UNKNOWN_METHOD___;
  }

  SMOP_RELEASE(interpreter,invocant);
  SMOP_RELEASE(interpreter,capture);
  return ret;
}
static SMOP__Object* ri_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if ((SMOP__Object*)responder != obj) {
    smop_lowlevel_wrlock(obj);
    ((SMOP_LOWLEVEL_INTERNAL*)obj->data)->ref_cnt++;
    smop_lowlevel_unlock(obj);
  }
  return obj;
}

static SMOP__Object* ri_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if ((SMOP__Object*)responder != obj) {
    smop_lowlevel_wrlock(obj);
    ((SMOP_LOWLEVEL_INTERNAL*)obj->data)->ref_cnt--;
    int destroy = ((SMOP_LOWLEVEL_INTERNAL*)obj->data)->ref_cnt <= 0;
    smop_lowlevel_unlock(obj);

    if (destroy) {
      smop_lowlevel_free(obj);
#ifdef SMOP_LOWLEVEL_MEM_TRACE
      smop_mem_trace_del(obj);
#endif
    }
  }
}


void smop_ri_init() {
  SMOP__RI = malloc(sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__RI)->MESSAGE = message;
  ((SMOP__ResponderInterface*)SMOP__RI)->REFERENCE = ri_reference;//smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__RI)->RELEASE = ri_release;//smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__RI)->id = "meta RI";
  SMOP__RI->RI = (SMOP__ResponderInterface*)SMOP__RI;
}

void smop_ri_destr() {
  free(SMOP__RI);
}

SMOP__Object* SMOP__RI__create(
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
  char *id
  ) {
    SMOP__Object* ret = smop_lowlevel_alloc(sizeof(SMOP__ResponderInterface));
    SMOP__ResponderInterface* ri = (SMOP__ResponderInterface*)ret;
    ri->RI = NULL; //(SMOP__ResponderInterface*)SMOP__RI;
    ri->MESSAGE = MESSAGE;
    ri->REFERENCE = REFERENCE;
    ri->RELEASE = RELEASE;
    ri->id = id;
    return ret;
}
