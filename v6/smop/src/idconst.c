/* Here is the implementation of the idconst type, which is used to
 * create the pool of constant identifiers that are needed to
 * bootstrap smop.
 */
#include <stdlib.h>
#include <string.h>
#include <smop.h>
#include <assert.h>
#include "smop_internal.h"

#include "idconst_decl_all.h"

/* The constant identifiers are not subject to garbage collection,
 * they are used as-is all the time. In fact, the string is saved on
 * them just to enable the stringification in the high-level.
 */

static SMOP__ResponderInterface* SMOP__NATIVE__idconst_RI;

static SMOP__Object* idconst_message(SMOP__Object* stack,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  // TODO
  return NULL;
}

static SMOP__Object* idconst_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  return obj;
}

static SMOP__Object* idconst_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  return obj;
}

void smop_idconst_init() {

  // create the responder interface
  
  SMOP__NATIVE__idconst_RI = calloc(1,sizeof(SMOP__ResponderInterface));
  assert(SMOP__NATIVE__idconst_RI);
  SMOP__NATIVE__idconst_RI->MESSAGE = idconst_message;
  SMOP__NATIVE__idconst_RI->REFERENCE = idconst_reference;
  SMOP__NATIVE__idconst_RI->RELEASE = idconst_release;

  // initialize the constants
#include "idconst_init_all.c"
  
}



void smop_idconst_destr() {

  // destroy the constants.
#include "idconst_destr_all.c"
  
  // destroy the responder interface.
  free(SMOP__NATIVE__idconst_RI);

}

void SMOP__NATIVE__idconst_free(SMOP__Object* value) {
  free(value->data);
  free(value);
}

SMOP__Object* SMOP__NATIVE__idconst_create(char* value) {
  SMOP__Object* ret = calloc(1,sizeof(SMOP__Object*));
  assert(ret);
  ret->RI = SMOP__NATIVE__idconst_RI;
  ret->data = calloc(1,strlen(value));
  assert(ret->data);
  strcpy(ret->data, value);
  return ret;
}
