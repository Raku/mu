#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <smop/base.h>
#include <smop/s0native.h>

SMOP__Object* SMOP__NATIVE__bool_true;
SMOP__Object* SMOP__NATIVE__bool_false;


void smop_native_bool_init() {

  SMOP__ResponderInterface* ri = (SMOP__ResponderInterface*)calloc(1,sizeof(SMOP__ResponderInterface));

  ri->MESSAGE = smop_placeholder_message;
  ri->REFERENCE = smop_noop_reference;
  ri->RELEASE = smop_noop_release;
  ri->WEAKREF = smop_noop_weakref;
  ri->id = "Native Boolean";
  ri->RI = (SMOP__ResponderInterface*)SMOP__metaRI;

  SMOP__NATIVE__bool_true = calloc(1,sizeof(SMOP__Object));
  SMOP__NATIVE__bool_true->RI = ri;

  SMOP__NATIVE__bool_false = calloc(1,sizeof(SMOP__Object));
  SMOP__NATIVE__bool_false->RI = ri;
  
}

void smop_native_bool_destr() {
  SMOP__ResponderInterface* ri = SMOP_RI(SMOP__NATIVE__bool_false);
  free(SMOP__NATIVE__bool_false);
  free(SMOP__NATIVE__bool_true);
  free(ri);
}
