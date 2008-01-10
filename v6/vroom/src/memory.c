#include "vroom.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

#ifdef VROOM_MEM_TRACE

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

void vroom_mem_trace_add(void* address) {

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

void vroom_mem_trace_del(void* address) {
  int pos = trace_list_find(address,0,trace_list_size);
  memmove(&trace_list[pos],&trace_list[pos+1], sizeof(void*)*(trace_list_size - pos));
  trace_list[trace_list_size - 1] = 0;
  trace_list_size--;

}

#endif

void vroom_memory_init() {

#ifdef VROOM_MEM_TRACE
  trace_list_alloc = 1024;
  trace_list = calloc(trace_list_alloc, sizeof(void*));
  trace_list_size = 0;
  assert(trace_list);
#endif

}

void vroom_memory_destr() {

#ifdef VROOM_MEM_TRACE
  if (trace_list_size > 0) {
    int i;
    for (i = 0; i < trace_list_size; i++) {
      fprintf(stderr,"Undestroyed variable: %p\n",trace_list[i]);
    }
  }
#endif

}

VROOM__CORE__Value* vroom_value_alloc(int size) {
  VROOM__CORE__Value* y = calloc(1,size);
  assert(y);
  y->ref_cnt = 1;
  y->rwlock = calloc(1,sizeof(pthread_rwlock_t));
  assert(y->rwlock);
  assert(pthread_rwlock_init(y->rwlock, NULL) == 0);
#ifdef VROOM_MEM_TRACE
  vroom_mem_trace_add(y);
#endif
  return y;
}

VROOM__CORE__Value* vroom_value_refcnt_inc(VROOM__CORE__Value* value) {
  vroom_value_wrlock(value);
  value->ref_cnt++;
  vroom_value_unlock(value);
  return value;
}

VROOM__CORE__Value* vroom_value_refcnt_dec(VROOM__CORE__Value* value) {
  vroom_value_wrlock(value);
  value->ref_cnt--;
  if (value->ref_cnt <= 0) {
#ifdef VROOM_MEM_TRACE
    vroom_mem_trace_del(value);
#endif
    vroom_value_unlock(value);
    VROOM_DESTR(value);
    vroom_value_wrlock(value);
    if (value->dispatcher) {
      vroom_value_refcnt_dec((VROOM__CORE__Value*)value->dispatcher);
    }
    pthread_rwlock_destroy(value->rwlock);
    free(value->rwlock);
    free(value);
    return NULL;
  } else {
    vroom_value_unlock(value);
    return value;
  }
}

void vroom_value_rdlock(VROOM__CORE__Value* value) {
  assert(pthread_rwlock_rdlock(value->rwlock) == 0);
}
void vroom_value_wrlock(VROOM__CORE__Value* value) {
  assert(pthread_rwlock_wrlock(value->rwlock) == 0);
}
void vroom_value_unlock(VROOM__CORE__Value* value) {
  assert(pthread_rwlock_unlock(value->rwlock) == 0);
}
