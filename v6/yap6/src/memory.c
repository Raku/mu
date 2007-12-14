#include "yap6.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

#ifdef YAP6_MEM_TRACE

static int trace_list_alloc;
static int trace_list_size;
static void** trace_list;

int trace_list_find(void* address, int start, int end) {
  if (start == end) {
    return start;
  } else {
    int middle = (end + start) / 2;
    if (address < trace_list[middle]) {
      return trace_list_find(address, start, middle);
    } else if (address > trace_list[middle]) {
      if (start == middle) {
        return end;
      } else {
        return trace_list_find(address, middle, end);
      }
    } else {
      return middle;
    }
  }
}

void yap6_mem_trace_add(void* address) {

  if (trace_list_alloc <= trace_list_size + 1) {
    trace_list = realloc(trace_list, sizeof(void*) * (trace_list_alloc + 1024));
    assert(trace_list);
    trace_list_alloc += 1024;
    memset(&trace_list[trace_list_size], 0, trace_list_alloc - trace_list_size);
  }

  int pos = 0;
  if (trace_list_size > 0) {
    pos = trace_list_find(address,0,trace_list_size);
    memmove(&trace_list[pos+1],&trace_list[pos], sizeof(void*)*(trace_list_size - pos));
  }
  trace_list[pos] = address;


  trace_list_size++;
}

void yap6_mem_trace_del(void* address) {
  int pos = trace_list_find(address,0,trace_list_size);
  memmove(&trace_list[pos],&trace_list[pos+1], sizeof(void*)*(trace_list_size - pos));
  trace_list[trace_list_size - 1] = 0;
  trace_list_size--;

}

#endif

void yap6_memory_init() {

#ifdef YAP6_MEM_TRACE
  trace_list_alloc = 1024;
  trace_list = calloc(trace_list_alloc, sizeof(void*));
  trace_list_size = 0;
  assert(trace_list);
#endif

}

void yap6_memory_destr() {

#ifdef YAP6_MEM_TRACE
  if (trace_list_size > 0) {
    int i;
    for (i = 0; i < trace_list_size; i++) {
      fprintf(stderr,"Undestroyed variable: %p\n",trace_list[i]);
    }
  }
#endif

}

YAP6__CORE__Value* yap6_value_alloc(int size) {
  YAP6__CORE__Value* y = calloc(1,size);
  assert(y);
  y->ref_cnt = 1;
  y->rwlock = calloc(1,sizeof(pthread_rwlock_t));
  assert(y->rwlock);
  assert(pthread_rwlock_init(y->rwlock, NULL) == 0);
#ifdef YAP6_MEM_TRACE
  yap6_mem_trace_add(y);
#endif
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
#ifdef YAP6_MEM_TRACE
    yap6_mem_trace_del(value);
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
