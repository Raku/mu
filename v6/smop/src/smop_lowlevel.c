/* This is the lowlevel implementation of the basic object management
 * which is the default object management for the smop responder
 * interfaces. As explained in the public headers, this doesn't mean
 * that using this is mandatory, it is available so you can use it in
 * your custom responder interfaces.
 */

#include <smop.h>
#include <smop_lowlevel.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>

/* The SMOP_LOWLEVEL_MEM_TRACE define will enable a trace on the
 * allocs and frees of your objecs, and should give you a warning when
 * this objects are still left during vm shutdown.
 */
#ifdef SMOP_LOWLEVEL_MEM_TRACE

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


void smop_lowlevel_init() {
#ifdef SMOP_LOWLEVEL_MEM_TRACE
  trace_list_alloc = 1024;
  trace_list = calloc(trace_list_alloc, sizeof(void*));
  trace_list_size = 0;
  assert(trace_list);
#endif
}

void smop_lowlevel_destr() {
#ifdef SMOP_LOWLEVEL_MEM_TRACE
  if (trace_list_size > 0) {
    int i;
    for (i = 0; i < trace_list_size; i++) {
      fprintf(stderr,"Undestroyed variable: %p\n",trace_list[i]);
    }
  }
#endif
}

typedef struct SMOP_LOWLEVEL_INTERNAL {
  int ref_cnt;
  pthread_rwlock_t* rwlock;
} SMOP_LOWLEVEL_INTERNAL;


SMOP__Object* smop_lowlevel_alloc(int size) {
  SMOP__Object* y = calloc(1,size);
  assert(y);
  SMOP_LOWLEVEL_INTERNAL* internal = calloc(1,sizeof(SMOP_LOWLEVEL_INTERNAL));
  assert(internal);
  y->data = internal;
  internal->ref_cnt = 1;
  internal->rwlock = calloc(1,sizeof(pthread_rwlock_t));
  assert(internal->rwlock);
  assert(pthread_rwlock_init(internal->rwlock, NULL) == 0);
#ifdef SMOP_LOWLEVEL_MEM_TRACE
  smop_mem_trace_add(y);
#endif
  return y;
}

SMOP__Object* smop_lowlevel_refcnt_inc(SMOP__Object* interpreter, SMOP__ResponderInterface* ri, SMOP__Object* value) {
  smop_lowlevel_wrlock(value);
  ((SMOP_LOWLEVEL_INTERNAL*)value->data)->ref_cnt++;
  smop_lowlevel_unlock(value);
  return value;
}

SMOP__Object* smop_lowlevel_refcnt_dec(SMOP__Object* interpreter, SMOP__ResponderInterface* ri, SMOP__Object* value) {
  smop_lowlevel_wrlock(value);
  ((SMOP_LOWLEVEL_INTERNAL*)value->data)->ref_cnt--;
  if (((SMOP_LOWLEVEL_INTERNAL*)value->data)->ref_cnt <= 0) {
#ifdef SMOP_LOWLEVEL_MEM_TRACE
    smop_mem_trace_del(value);
#endif
    smop_lowlevel_unlock(value);

    // This is where the continuation will be manipulated as described in the .h file.
    SMOP__Object* current = SMOP_DISPATCH(interpreter, SMOP_RI(interpreter), SMOP__ID__current,
                                          smop__intptr__invocant_capture_new(interpreter));
    SMOP__Object* continuation = q:sm0p {
      $current;
      $interpreter;
      $value.DESTROYALL();
      SMOP__STACK__Operators.move_capturize(|SMOP__STACK__OPCAPTURE_Move_Capturize.new(2,(3),(),3));
      SMOP__STACK__Operators.forget();
      SMOP__STACK__Operators.free(|$value);
      $interpreter.goto()
    };
    SMOP_DISPATCH(interpreter, SMOP_RI(interpreter), SMOP__ID__goto, smop__intptr__goto_capture_new(interpreter, continuation));
    return NULL;
  } else {
    smop_lowlevel_unlock(value);
    return value;
  }
}

void smop_lowlevel_rdlock(SMOP__Object* value) {
  assert(pthread_rwlock_rdlock(((SMOP_LOWLEVEL_INTERNAL*)value->data)->rwlock) == 0);
}
void smop_lowlevel_wrlock(SMOP__Object* value) {
  assert(pthread_rwlock_wrlock(((SMOP_LOWLEVEL_INTERNAL*)value->data)->rwlock) == 0);
}
void smop_lowlevel_unlock(SMOP__Object* value) {
  assert(pthread_rwlock_unlock(((SMOP_LOWLEVEL_INTERNAL*)value->data)->rwlock) == 0);
}
