#include "smop.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

#ifdef SMOP_MEM_TRACE

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

void smop_mem_trace_add(void* address) {

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

void smop_mem_trace_del(void* address) {
  int pos = trace_list_find(address,0,trace_list_size);
  memmove(&trace_list[pos],&trace_list[pos+1], sizeof(void*)*(trace_list_size - pos));
  trace_list[trace_list_size - 1] = 0;
  trace_list_size--;

}

#endif

void smop_memory_init() {

#ifdef SMOP_MEM_TRACE
  trace_list_alloc = 1024;
  trace_list = calloc(trace_list_alloc, sizeof(void*));
  trace_list_size = 0;
  assert(trace_list);
#endif

}

void smop_memory_destr() {

#ifdef SMOP_MEM_TRACE
  if (trace_list_size > 0) {
    int i;
    for (i = 0; i < trace_list_size; i++) {
      fprintf(stderr,"Undestroyed variable: %p\n",trace_list[i]);
    }
  }
#endif

}

SMOP__CORE__Value* smop_value_alloc(int size) {
  SMOP__CORE__Value* y = calloc(1,size);
  assert(y);
  y->ref_cnt = 1;
  y->rwlock = calloc(1,sizeof(pthread_rwlock_t));
  assert(y->rwlock);
  assert(pthread_rwlock_init(y->rwlock, NULL) == 0);
#ifdef SMOP_MEM_TRACE
  smop_mem_trace_add(y);
#endif
  return y;
}

SMOP__CORE__Value* smop_value_refcnt_inc(SMOP__CORE__Value* value) {
  smop_value_wrlock(value);
  value->ref_cnt++;
  smop_value_unlock(value);
  return value;
}

SMOP__CORE__Value* smop_value_refcnt_dec(SMOP__CORE__Value* value) {
  smop_value_wrlock(value);
  value->ref_cnt--;
  if (value->ref_cnt <= 0) {
#ifdef SMOP_MEM_TRACE
    smop_mem_trace_del(value);
#endif
    smop_value_unlock(value);
    SMOP_DESTR(value);
    smop_value_wrlock(value);
    if (value->dispatcher) {
      smop_value_refcnt_dec((SMOP__CORE__Value*)value->dispatcher);
    }
    pthread_rwlock_destroy(value->rwlock);
    free(value->rwlock);
    free(value);
    return NULL;
  } else {
    smop_value_unlock(value);
    return value;
  }
}

void smop_value_rdlock(SMOP__CORE__Value* value) {
  assert(pthread_rwlock_rdlock(value->rwlock) == 0);
}
void smop_value_wrlock(SMOP__CORE__Value* value) {
  assert(pthread_rwlock_wrlock(value->rwlock) == 0);
}
void smop_value_unlock(SMOP__CORE__Value* value) {
  assert(pthread_rwlock_unlock(value->rwlock) == 0);
}
