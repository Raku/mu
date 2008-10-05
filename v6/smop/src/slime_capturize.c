#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_slime.h>

SMOP__Object* SMOP__SLIME__Capturize;

typedef struct smop_slime_capturize_struct {
  SMOP__Object__BASE
  int invocant;
  int n_positional;
  int* positional;
  int n_named;
  int* named;
  int target;
} smop_slime_capturize_struct;

static SMOP__Object* capturize_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  // todo
  if (identifier == SMOP__ID__DESTROYALL) {
    smop_lowlevel_wrlock(capture);
    ((smop_slime_capturize_struct*)capture)->n_positional = 0;
    ((smop_slime_capturize_struct*)capture)->n_named = 0;
    int* p = ((smop_slime_capturize_struct*)capture)->positional;
    ((smop_slime_capturize_struct*)capture)->positional = NULL;
    int* n = ((smop_slime_capturize_struct*)capture)->named;
    ((smop_slime_capturize_struct*)capture)->named = NULL;
    smop_lowlevel_unlock(capture);
    free(p); free(n);
  } else {
    SMOP_RELEASE(interpreter,capture);
  }

  return capture;
}

static SMOP__Object* capturize_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if ((SMOP__Object*)responder != obj) {
    smop_lowlevel_refcnt_inc(interpreter, responder, obj);
  }
  return obj;
}

static SMOP__Object* capturize_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if ((SMOP__Object*)responder != obj) {
    smop_lowlevel_refcnt_dec(interpreter, responder, obj);
  }
  return obj;
}

void smop_slime_capturize_init() {
  SMOP__SLIME__Capturize = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__SLIME__Capturize)->MESSAGE = capturize_message;
  ((SMOP__ResponderInterface*)SMOP__SLIME__Capturize)->REFERENCE = capturize_reference;
  ((SMOP__ResponderInterface*)SMOP__SLIME__Capturize)->RELEASE = capturize_release;
  ((SMOP__ResponderInterface*)SMOP__SLIME__Capturize)->id = "SMOP SLIME Capturize";
}

void smop_slime_capturize_destr() {
  free(SMOP__SLIME__Capturize);
}

SMOP__Object* SMOP__SLIME__Capturize_create(int invocant, int* positional, int* named, int target) {
  SMOP__Object* ret = smop_lowlevel_alloc(sizeof(smop_slime_capturize_struct));
  assert(ret);
  smop_slime_capturize_struct* cap = (smop_slime_capturize_struct*)ret;
  cap->RI = (SMOP__ResponderInterface*)SMOP__SLIME__Capturize;
  cap->invocant = invocant;
  cap->target = target;

  if (positional) {
    cap->n_positional = -1;
    while (positional && positional[++(cap->n_positional)]);
    cap->positional = malloc(sizeof(int) * cap->n_positional);
    memcpy(cap->positional,positional,sizeof(int)*cap->n_positional);
  } else {
    cap->n_positional = 0;
  }

  if (named) {
    cap->n_named = -1;
    while (named[++(cap->n_named)]);
    cap->named = malloc(sizeof(int) * cap->n_named);
    memcpy(cap->named,named,sizeof(int)*cap->n_named);
  } else {
    cap->n_named = 0;
  }
  return ret;
}

int SMOP__SLIME__Capturize_invocant(SMOP__Object* obj) {
  smop_lowlevel_rdlock(obj);
  int ret = ((smop_slime_capturize_struct*)obj)->invocant;
  smop_lowlevel_unlock(obj);
  return ret;
}

int* SMOP__SLIME__Capturize_positional(SMOP__Object* obj, int* retsize) {
  smop_lowlevel_rdlock(obj);
  *retsize = ((smop_slime_capturize_struct*)obj)->n_positional;
  int* ret = malloc(sizeof(int) * *retsize);
  memcpy(ret, ((smop_slime_capturize_struct*)obj)->positional,
         sizeof(int)*((smop_slime_capturize_struct*)obj)->n_positional);
  smop_lowlevel_unlock(obj);
  return ret;

}

int* SMOP__SLIME__Capturize_named(SMOP__Object* obj, int* retsize) {
  smop_lowlevel_rdlock(obj);
  *retsize = ((smop_slime_capturize_struct*)obj)->n_named;
  int* ret = malloc(sizeof(int) * *retsize);
  memcpy(ret, ((smop_slime_capturize_struct*)obj)->named,
         sizeof(int)*((smop_slime_capturize_struct*)obj)->n_named);
  smop_lowlevel_unlock(obj);
  return ret;
}

int SMOP__SLIME__Capturize_target(SMOP__Object* obj) {
  smop_lowlevel_rdlock(obj);
  int ret = ((smop_slime_capturize_struct*)obj)->target;
  smop_lowlevel_unlock(obj);
  return ret;
}
