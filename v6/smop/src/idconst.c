/* Here is the implementation of the idconst type, which is used to
 * create the pool of constant identifiers that are needed to
 * bootstrap smop.
 */
#include <stdlib.h>
#include <string.h>
#include <smop.h>
#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <smop_s1p.h>
#include "smop_internal.h"

static SMOP__Object** constlist;
static int constlist_size;
static pthread_rwlock_t constlist_lock;

#include "idconst_decl_all.h"

/* The constant identifiers are not subject to garbage collection,
 * they are used as-is all the time. In fact, the string is saved on
 * them just to enable the stringification in the high-level.
 */

static SMOP__ResponderInterface* SMOP__NATIVE__idconst_RI;

typedef struct SMOP__NATIVE__idconst_data {
  int size;
  char content[];
} SMOP__NATIVE__idconst_data;

static int cmp_idconst(const void* p1, const void* p2) {
  if (!p1 && !p2) {
    return 0;
  } else if (p1 && !p2) {
    return 1;
  } else if (!p1 && p2) {
    return -1;
  } else {
    SMOP__Object* o1 = *(SMOP__Object**)p1;
    SMOP__Object* o2 = *(SMOP__Object**)p2;
    SMOP__NATIVE__idconst_data* n1 = (SMOP__NATIVE__idconst_data*)o1->data;
    SMOP__NATIVE__idconst_data* n2 = (SMOP__NATIVE__idconst_data*)o2->data;
    if (n1->size > n2->size) {
      int r = strncmp(n1->content, n2->content, n2->size);
      return r ? r : -1;
    } else if (n2->size > n1->size) {
      int r = strncmp(n1->content, n2->content, n1->size);
      return r ? r : 1;
    } else {
      return strncmp(n1->content, n2->content, n1->size);
    }
  }
}

static SMOP__Object* idconst_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  ___NATIVE_CAPTURE_ONLY___;
  ___CONST_IDENTIFIER_ONLY___;
  ___INVOCANT_RI_SHOULD_MATCH___;

  SMOP__Object* ret = SMOP__NATIVE__bool_false;

  if (SMOP__ID__FETCH == identifier) {
    ___VALUE_FETCH___;
  } else if (SMOP__ID__STORE == identifier) {
    ___VALUE_STORE___;
  } else {
    ___UNKNOWN_METHOD___;
  }

  SMOP_RELEASE(interpreter,invocant);
  SMOP_RELEASE(interpreter,capture);
  return ret;
}

static SMOP__Object* idconst_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  return obj;
}

static SMOP__Object* idconst_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  return obj;
}

char* SMOP__NATIVE__idconst_fetch(SMOP__Object* value, int* retsize) {
  assert(value->RI == SMOP__NATIVE__idconst_RI);
  *retsize = ((SMOP__NATIVE__idconst_data*)value->data)->size;
  return ((SMOP__NATIVE__idconst_data*)value->data)->content;
}

void SMOP__NATIVE__idconst_free(SMOP__Object* value) {
  free(value->data);
  free(value);
}

SMOP__Object* SMOP__NATIVE__idconst_createn_nolist(const char* value, int size) {
  SMOP__Object* ret = calloc(1,sizeof(SMOP__Object));
  assert(ret);
  ret->RI = SMOP__NATIVE__idconst_RI;
  ret->data = calloc(1, size + sizeof(SMOP__NATIVE__idconst_data));
  assert(ret->data);
  ((SMOP__NATIVE__idconst_data*)ret->data)->size = size;
  strncpy(((SMOP__NATIVE__idconst_data*)ret->data)->content, value, size);
  return ret;
}

SMOP__Object* SMOP__NATIVE__idconst_create_nolist(const char* value) {
  int size = strlen(value);
  return SMOP__NATIVE__idconst_createn_nolist(value,size);
}


SMOP__Object* SMOP__NATIVE__idconst_createn(const char* value, int size) {
  SMOP__Object* candidate = SMOP__NATIVE__idconst_createn_nolist(value,size);

  assert(pthread_rwlock_rdlock(&constlist_lock) == 0);
  SMOP__Object** ret = bsearch(&candidate, constlist, constlist_size, sizeof(SMOP__Object*), cmp_idconst);
  assert(pthread_rwlock_unlock(&constlist_lock) == 0);

  if (ret) {
    SMOP__NATIVE__idconst_free(candidate);
    return *ret;
  } else {

    assert(pthread_rwlock_wrlock(&constlist_lock) == 0);

    constlist_size++;

    constlist = realloc(constlist, constlist_size * sizeof(SMOP__Object*));
    assert(constlist);

    constlist[constlist_size - 1] = candidate;

    qsort(constlist, constlist_size, sizeof(SMOP__Object*), cmp_idconst);

    assert(pthread_rwlock_unlock(&constlist_lock) == 0);

    return candidate;
  }
}

SMOP__Object* SMOP__NATIVE__idconst_create(const char* value) {
  int size = strlen(value);
  return SMOP__NATIVE__idconst_createn(value,size);
}


void smop_idconst_init() {

  // create the responder interface
  
  SMOP__NATIVE__idconst_RI = calloc(1,sizeof(SMOP__ResponderInterface));
  assert(SMOP__NATIVE__idconst_RI);
  SMOP__NATIVE__idconst_RI->MESSAGE = idconst_message;
  SMOP__NATIVE__idconst_RI->REFERENCE = idconst_reference;
  SMOP__NATIVE__idconst_RI->RELEASE = idconst_release;
  SMOP__NATIVE__idconst_RI->id = "Constant Identifier";

  // initialize the constants
#include "idconst_init_all.c"

  qsort(constlist, constlist_size, sizeof(SMOP__Object*), cmp_idconst);

  assert(pthread_rwlock_init(&constlist_lock, NULL) == 0);
  
}



void smop_idconst_destr() {
  
  int i;
  for (i = constlist_size - 1; i >= 0; i--) {
    SMOP__NATIVE__idconst_free(constlist[i]);
    constlist[i] = NULL;
  }

  // destroy the responder interface.
  free(SMOP__NATIVE__idconst_RI);

  free(constlist); constlist_size = 0;
  pthread_rwlock_destroy(&constlist_lock);

}

