#include <stdio.h>
#include <stdlib.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>
#include <smop_slime.h>

SMOP__Object* SMOP__S1P__Attribute;
typedef struct SMOP__S1P__Attribute_struct {
  SMOP__Object__BASE
  SMOP__Object* name;
  SMOP__Object* private_name;
  SMOP__Object* container_type;
} SMOP__S1P__Attribute_struct;

static SMOP__Object* smop_s1p_attribute_message(SMOP__Object* interpreter,
                                                SMOP__ResponderInterface* responder,
                                                SMOP__Object* identifier,
                                                SMOP__Object* capture) {
  ___NATIVE_CAPTURE_ONLY___;
  ___CONST_IDENTIFIER_ONLY___;
  ___INVOCANT_RI_SHOULD_MATCH___;

  SMOP__Object* ret = SMOP__NATIVE__bool_false;

  if (identifier == SMOP__ID__new) {

  } else if (identifier == SMOP__ID__DESTROYALL) {
    
  }

  return ret;
}

void smop_s1p_scalar_init() {
  SMOP__S1P__Attribute = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__Attribute)->MESSAGE = smop_s1p_attribute_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Attribute)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Attribute)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Attribute)->id = "S1P Attribute";
}

void smop_s1p_scalar_destr() {
  free(SMOP__S1P__Attribute);
}
