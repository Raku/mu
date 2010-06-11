/* Here is the implementation of the idconst type, which is used to
 * create the pool of constant identifiers that are needed to
 * bootstrap smop.
 */
#include <smop/base.h>
#include <smop/s0native.h>

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef SMOP_LOCKING
#include <pthread.h>
#endif
#include <stdio.h>

static SMOP__Object** constlist;
static int constlist_size;
#ifdef SMOP_LOCKING
static pthread_rwlock_t constlist_lock;
#endif


/* The constant identifiers are not subject to garbage collection,
 * they are used as-is all the time. In fact, the string is saved on
 * them just to enable the stringification in the high-level.
 */

SMOP__ResponderInterface* SMOP__NATIVE__idconst_RI;

typedef struct SMOP__NATIVE__idconst_struct {
  SMOP__Object__BASE
  int size;
  char* content;
} SMOP__NATIVE__idconst_struct;

static int cmp_idconst(const void* p1, const void* p2) {
  if (!p1 && !p2) {
    return 0;
  } else if (p1 && !p2) {
    return 1;
  } else if (!p1 && p2) {
    return -1;
  } else {
    SMOP__NATIVE__idconst_struct* o1 = *(SMOP__NATIVE__idconst_struct**)p1;
    SMOP__NATIVE__idconst_struct* o2 = *(SMOP__NATIVE__idconst_struct**)p2;
    if (o1->size > o2->size) {
      int r = strncmp(o1->content, o2->content, o2->size);
      return r ? r : -1;
    } else if (o2->size > o1->size) {
      int r = strncmp(o1->content, o2->content, o1->size);
      return r ? r : 1;
    } else {
      return strncmp(o1->content, o2->content, o1->size);
    }
  }
}



char* SMOP__NATIVE__idconst_fetch(SMOP__Object* value, int* retsize) {
  assert(value->RI == SMOP__NATIVE__idconst_RI);
  *retsize = ((SMOP__NATIVE__idconst_struct*)value)->size;
  return ((SMOP__NATIVE__idconst_struct*)value)->content;
}

char* SMOP__NATIVE__idconst_fetch_with_null(SMOP__Object* value,int* retsize) {
    char* str = SMOP__NATIVE__idconst_fetch(value,retsize);
    char* str_with_null = malloc(sizeof(char) * (*retsize+1));
    strncpy(str_with_null,str,*retsize);
    str_with_null[*retsize] = '\0';
    return str_with_null;
}

void SMOP__NATIVE__idconst_free(SMOP__Object* value) {
  free(((SMOP__NATIVE__idconst_struct*)value)->content);
  free(value);
}


static SMOP__Object* SMOP__NATIVE__idconst_createn_nolist(const char* value, int size) {
  SMOP__NATIVE__idconst_struct* ret = (SMOP__NATIVE__idconst_struct*) malloc(sizeof(SMOP__NATIVE__idconst_struct));
  ret->RI = SMOP__NATIVE__idconst_RI;
  ret->size = size;
  ret->content = malloc(sizeof(char) * size);
  strncpy(ret->content, value, size);
  return (SMOP__Object*) ret;
}

SMOP__Object* SMOP__NATIVE__idconst_create(const char* value) {
  return SMOP__NATIVE__idconst_createn(value,strlen(value));
}
SMOP__Object* SMOP__NATIVE__idconst_createn(const char* value, int size) {
  SMOP__Object* candidate = SMOP__NATIVE__idconst_createn_nolist(value,size);

#ifdef SMOP_LOCKING
  assert(pthread_rwlock_rdlock(&constlist_lock) == 0);
#endif
  SMOP__Object** ret = bsearch(&candidate, constlist, constlist_size, sizeof(SMOP__Object*), cmp_idconst);
#ifdef SMOP_LOCKING
  assert(pthread_rwlock_unlock(&constlist_lock) == 0);
#endif


  if (ret) {
    SMOP__NATIVE__idconst_free(candidate);
    return *ret;
  } else {

#ifdef SMOP_LOCKING
    assert(pthread_rwlock_wrlock(&constlist_lock) == 0);
#endif

    constlist_size++;

    constlist = realloc(constlist, constlist_size * sizeof(SMOP__Object*));
    assert(constlist);

    constlist[constlist_size - 1] = candidate;

    qsort(constlist, constlist_size, sizeof(SMOP__Object*), cmp_idconst);

#ifdef SMOP_LOCKING
    assert(pthread_rwlock_unlock(&constlist_lock) == 0);
#endif

    return candidate;
  }
}


void smop_idconst_init() {

  // create the responder interface
  
  SMOP__NATIVE__idconst_RI = calloc(1,sizeof(SMOP__ResponderInterface));
  assert(SMOP__NATIVE__idconst_RI);
  SMOP__NATIVE__idconst_RI->MESSAGE = smop_placeholder_message;
  SMOP__NATIVE__idconst_RI->REFERENCE = smop_noop_reference;
  SMOP__NATIVE__idconst_RI->RELEASE = smop_noop_release;
  SMOP__NATIVE__idconst_RI->WEAKREF = smop_noop_weakref;
  SMOP__NATIVE__idconst_RI->id = "Constant Identifier";
  SMOP__NATIVE__idconst_RI->RI = (SMOP__ResponderInterface *)SMOP__metaRI;

  qsort(constlist, constlist_size, sizeof(SMOP__Object*), cmp_idconst);

#ifdef SMOP_LOCKING
  assert(pthread_rwlock_init(&constlist_lock, NULL) == 0);
#endif
  
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
#ifdef SMOP_LOCKING
  pthread_rwlock_destroy(&constlist_lock);
#endif

}

