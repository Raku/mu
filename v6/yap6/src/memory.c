#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#ifdef YAP6_MEM_TRACE
#include <stdio.h>
#endif

YAP6__CORE__Value* yap6_value_alloc(int size) {
  YAP6__CORE__Value* y = calloc(1,size);
  assert(y);
  y->ref_cnt = 1;
  y->rwlock = calloc(1,sizeof(pthread_rwlock_t));
  assert(y->rwlock);
  assert(pthread_rwlock_init(y->rwlock, NULL) == 0);
#ifdef YAP6_MEM_TRACE
  printf("# = %p (1)\n",y);
#endif
  return y;
}

void yap6_value_refcnt_inc(YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
  value->ref_cnt++;
#ifdef YAP6_MEM_TRACE
  printf("# + %p (%d)\n",value,value->ref_cnt);
#endif
  yap6_value_unlock(value);
}

void yap6_value_refcnt_dec(YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
  value->ref_cnt--;
#ifdef YAP6_MEM_TRACE
  printf("# - %p (%d) ",value,value->ref_cnt);
#endif
  if (value->ref_cnt <= 0) {
#ifdef YAP6_MEM_TRACE
    printf("X\n");
#endif
    yap6_value_unlock(value);
    YAP6_DESTR(value);
    yap6_value_wrlock(value);
    if (value->dispatcher) {
      yap6_value_refcnt_dec((YAP6__CORE__Value*)value->dispatcher);
    }
    pthread_rwlock_destroy(value->rwlock);
    free(value->rwlock);
    free(value);
  } else {
#ifdef YAP6_MEM_TRACE
    printf("\n");
#endif
    yap6_value_unlock(value);
  }
}

void yap6_value_rdlock(YAP6__CORE__Value* value) {
  assert(pthread_rwlock_rdlock(value->rwlock) == 0);
}
void yap6_value_wrlock(YAP6__CORE__Value* value) {
  assert(pthread_rwlock_wrlock(value->rwlock) == 0);
}
void yap6_value_unlock(YAP6__CORE__Value* value) {
  assert(pthread_rwlock_unlock(value->rwlock) == 0);
}
