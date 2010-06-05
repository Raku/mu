#include <stdlib.h>
#include <smop/base.h>
#include <smop/s0native.h>

SMOP__Object* SMOP__metaRI;


void smop_metaRI_init() {
  SMOP__ResponderInterface* ri = calloc(1,sizeof(SMOP__ResponderInterface));
  ri->MESSAGE = smop_placeholder_message;
  ri->REFERENCE = smop_noop_reference;
  ri->RELEASE = smop_noop_release;
  ri->WEAKREF = smop_noop_weakref;
  ri->id = "non-gc meta-RI";
  ri->RI = (SMOP__ResponderInterface*)ri;
  SMOP__metaRI = (SMOP__Object*)ri;
}

void smop_metaRI_destr() {
  free(SMOP__metaRI);
}
