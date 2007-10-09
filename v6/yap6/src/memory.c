#include "yap6.h"
#include <stdlib.h>
#include <assert.h>

YAP6__CORE__Value* yap6_value_alloc(int size) {
  YAP6__CORE__Value* y = calloc(1,size);
  assert(y);
  y->ref_cnt = 1;
  y->rwlock = calloc(1,sizeof(pthread_rwlock_t));
  assert(y->rwlock);
  assert(pthread_rwlock_init(y->rwlock, NULL) == 0);
  return y;
}

void yap6_value_refcnt_inc(YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
  value->ref_cnt++;
  yap6_value_unlock(value);
}

void yap6_value_refcnt_dec(YAP6__CORE__Value* value) {
  yap6_value_wrlock(value);
  value->ref_cnt--;
  if (value->ref_cnt <= 0) {
    // TODO: dispatch DESTR on value
    pthread_rwlock_t* foo = value->rwlock;
    free(value);
    pthread_rwlock_destroy(foo);
    free(foo);
  } else {
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
