#include <stdio.h>
#include <stdlib.h>
#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <pthread.h>
#include "weakref.h"

SMOP__Object* smop_nagc_alloc(size_t size) {
  if (size < sizeof(SMOP__NAGC__Object))
    abort();
  SMOP__NAGC__Object* y = calloc(1,size);
  if (!y)
    abort();

  y->ref_cnt = 1;
  y->rwlock = calloc(1,sizeof(pthread_rwlock_t));
  if (!y->rwlock)
    abort();
  if (pthread_rwlock_init(y->rwlock, NULL))
    abort();

  return (SMOP__Object*)y;
}

void smop_nagc_rdlock(SMOP__NAGC__Object* value) {
  if (pthread_rwlock_rdlock(value->rwlock)) {
    abort();
  }
}

void smop_nagc_wrlock(SMOP__NAGC__Object* value) {
  if (pthread_rwlock_wrlock(value->rwlock)) {
    abort();
  }
}

void smop_nagc_unlock(SMOP__NAGC__Object* value) {
  if (pthread_rwlock_unlock(value->rwlock)) {
    abort();
  }
}

SMOP__Object* smop_nagc_reference(SMOP__Object* interpreter,
                                  SMOP__ResponderInterface* responder,
                                  SMOP__Object* obj) {
  if ((SMOP__Object*)responder != obj) {
    smop_nagc_wrlock((SMOP__NAGC__Object*)obj);
    ((SMOP__NAGC__Object*)obj)->ref_cnt++;
    smop_nagc_unlock((SMOP__NAGC__Object*)obj);
  }
  return obj;
}


SMOP__Object* smop_nagc_release(SMOP__Object* interpreter,
                                SMOP__ResponderInterface* responder,
                                SMOP__Object* obj) {
  if ((SMOP__Object*)responder != obj) {
    int destroy = 0;
    smop_nagc_wrlock((SMOP__NAGC__Object*)obj);
    ((SMOP__NAGC__Object*)obj)->ref_cnt--;
    if (((SMOP__NAGC__Object*)obj)->ref_cnt == 0) {
      /*destroy = 1;*/
      ((SMOP__NAGC__Object*)obj)->ref_cnt = 999;
    }
    smop_nagc_unlock((SMOP__NAGC__Object*)obj);

    if (destroy) {
      SMOP__NAGC__ResponderInterface* ri = (SMOP__NAGC__ResponderInterface*)SMOP_RI(obj);
      smop_nagc_weakref_cleanup((SMOP__NAGC__Object*)obj);
      ri->DESTROYALL(interpreter, obj);
      pthread_rwlock_destroy(((SMOP__NAGC__Object*)obj)->rwlock);
      free(((SMOP__NAGC__Object*)obj)->rwlock);
      free(obj);
      return NULL;
    } else {
      return obj;
    }
  } else {
    return obj;
  }
}

SMOP__Object* smop_nagc_weakref(SMOP__Object* interpreter,
                                SMOP__ResponderInterface* responder,
                                SMOP__Object* obj) {
  return smop_nagc_weakref_create((SMOP__NAGC__Object*)obj);
}

void smop_nagc_init() {
  smop_nagc_weakref_init();
}

void smop_nagc_destr() {
  smop_nagc_weakref_destr();
}
