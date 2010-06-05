#include <stdio.h>
#include <stdlib.h>
#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/dump.h>

#ifdef SMOP_LOCKING
#include <pthread.h>
#endif

#include "weakref.h"

#ifdef SMOP_LEAK_TRACE
static SMOP__Object* leaks[20000];
#endif

SMOP__Object* smop_nagc_alloc(size_t size) {
  if (size < sizeof(SMOP__NAGC__Object))
    abort();
  SMOP__NAGC__Object* y = calloc(1,size);
  if (!y)
    abort();

  y->ref_cnt = 1;
#ifdef SMOP_LOCKING
  y->rwlock = calloc(1,sizeof(pthread_rwlock_t));
  if (!y->rwlock)
    abort();
  if (pthread_rwlock_init(y->rwlock, NULL))
    abort();
#endif

#ifdef SMOP_LEAK_TRACE
  int i = 0;
  while (leaks[i]) i++;
  leaks[i] = y;
#endif

  return (SMOP__Object*)y;
}

void smop_nagc_rdlock(SMOP__NAGC__Object* value) {
#ifdef SMOP_LOCKING
  if (pthread_rwlock_rdlock(value->rwlock)) {
    abort();
  }
#endif
}

void smop_nagc_wrlock(SMOP__NAGC__Object* value) {
#ifdef SMOP_LOCKING
  if (pthread_rwlock_wrlock(value->rwlock)) {
    abort();
  }
#endif
}

void smop_nagc_unlock(SMOP__NAGC__Object* value) {
#ifdef SMOP_LOCKING
  if (pthread_rwlock_unlock(value->rwlock)) {
    abort();
  }
#endif
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

static SMOP__Object* release_code(SMOP__Object* interpreter,
                                  SMOP__ResponderInterface* responder,
                                  SMOP__Object* obj,
                                  int shouldfree) {
  if ((SMOP__Object*)responder != obj) {
    int destroy = 0;
    smop_nagc_wrlock((SMOP__NAGC__Object*)obj);
    ((SMOP__NAGC__Object*)obj)->ref_cnt--;
    if (((SMOP__NAGC__Object*)obj)->ref_cnt == 0) {
      destroy = 1;
      ((SMOP__NAGC__Object*)obj)->ref_cnt = 999;
    }
    if (((SMOP__NAGC__Object*)obj)->ref_cnt < 0) {
      printf("ref_cnt = %d RI.id = %s\n",((SMOP__NAGC__Object*)obj)->ref_cnt,SMOP_RI(obj)->id);
    }
    smop_nagc_unlock((SMOP__NAGC__Object*)obj);

    if (destroy) {
      SMOP__NAGC__ResponderInterface* ri = (SMOP__NAGC__ResponderInterface*)SMOP_RI(obj);
      smop_nagc_weakref_cleanup((SMOP__NAGC__Object*)obj);
      ri->DESTROYALL(interpreter, obj);
      if (shouldfree) {
        smop_nagc_free((SMOP__NAGC__Object*)obj);
        return NULL;
      } else {
        return obj;
      }
    } else {
      return obj;
    }
  } else {
    return obj;
  }
}

void smop_nagc_free(SMOP__NAGC__Object* obj) {
#ifdef SMOP_LOCKING
  pthread_rwlock_destroy(((SMOP__NAGC__Object*)obj)->rwlock);
  free(((SMOP__NAGC__Object*)obj)->rwlock);
#endif
  free(obj);

#ifdef SMOP_LEAK_TRACE
  int i = 0;
  while (leaks[i] != obj) i++;
  leaks[i] = NULL;
#endif

}

SMOP__Object* smop_nagc_dump(SMOP__Object* interpreter,
                                SMOP__ResponderInterface* responder,
                                SMOP__Object* obj) {
  printf("smop_nagc_dump\n");
  return smop_dump_create((SMOP__Object*[]) {
      smop_dump_attr_create("RI"),
      smop_dump_obj_create(obj->RI),
      smop_dump_attr_create("ref_cnt"),
      smop_dump_int_create(((SMOP__NAGC__Object*)obj)->ref_cnt),
      NULL
  });
}

/* TODO refactor */
SMOP__Object* smop_nagc_release(SMOP__Object* interpreter,
                                SMOP__ResponderInterface* responder,
                                SMOP__Object* obj) {
  return release_code(interpreter,responder,obj,1);
}
SMOP__Object* smop_nagc_release_nofree(SMOP__Object* interpreter,
                                       SMOP__ResponderInterface* responder,
                                       SMOP__Object* obj) {
  return release_code(interpreter,responder,obj,0);
}

SMOP__Object* smop_nagc_weakref(SMOP__Object* interpreter,
                                SMOP__ResponderInterface* responder,
                                SMOP__Object* obj) {
  return smop_nagc_weakref_create((SMOP__NAGC__Object*)obj);
}

void smop_nagc_init() {
  smop_nagc_weakref_init();
  smop_nagc_ri_init();
}

void smop_nagc_destr() {
  smop_nagc_ri_destr();
  smop_nagc_weakref_destr();
#ifdef SMOP_LEAK_TRACE
  int i = 0;
  while (i < 18000) {
    if (leaks[i]) fprintf(stderr,"%p = %s#leak\n",leaks[i],leaks[i]->RI->id);
    i++;
  }
  leaks[i] = NULL;
#endif
}
