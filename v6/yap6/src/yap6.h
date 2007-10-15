
#ifndef __YAP6_H
#define __YAP6_H

#include <pthread.h>

// initialize all constants and low-level dispatchers...
extern void yap6_init();

// forward declarations
struct YAP6__CORE__Value; typedef struct YAP6__CORE__Value YAP6__CORE__Value;
struct YAP6__CORE__Scalar; typedef struct YAP6__CORE__Scalar YAP6__CORE__Scalar;
struct YAP6__CORE__Dispatcher; typedef struct YAP6__CORE__Dispatcher YAP6__CORE__Dispatcher;
struct YAP6__CORE__ScalarDispatcher; typedef struct YAP6__CORE__ScalarDispatcher YAP6__CORE__ScalarDispatcher;
struct YAP6__CORE__Capture; typedef struct YAP6__CORE__Capture YAP6__CORE__Capture;
struct YAP6__CORE__CaptureDispatcher; typedef struct YAP6__CORE__CaptureDispatcher YAP6__CORE__CaptureDispatcher;

/*
 * The YAP6__CORE__Oject struct represents any object in the YAP6 runtime.
 * The data of this object should be opaque for the users, the only
 * one that should know about it is the dispatcher. Every object must
 * have a dispatcher. If it doesn't, it is considered itself as one.
 */
struct YAP6__CORE__Value {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
};

/*
 * The YAP6__CORE__Dispatcher struct is the superclass of all dispatchers
 * implemented. As a dispatcher, it should have the member
 * "dispatcher" as null, and should have the member APPLY just after
 * it which is itself a pointer to the APPLY function of the
 * dispatcher.
 */
struct YAP6__CORE__Dispatcher {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  YAP6__CORE__Value* (*APPLY)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Capture* arguments,
                               YAP6__CORE__Value* wants);
  void               (*DESTR)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value);
  int                (*COMPR)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* other);
};

typedef struct YAP6__CORE__int {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  int value;  
} YAP6__CORE__int;

/* int support */
extern YAP6__CORE__Dispatcher* yap6_const_int_dispatcher;
extern void yap6_int_dispatcher_init();
extern YAP6__CORE__int* yap6_int_create(int initialvalue);
extern int yap6_int_lowlevel(YAP6__CORE__int* value);

typedef struct YAP6__CORE__double {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  double value;  
} YAP6__CORE__double;

typedef struct YAP6__CORE__string {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  char encoding[16];
  int byte_length;
  char content[];
} YAP6__CORE__String;

typedef struct YAP6__CORE__blob {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  int byte_length;
  char content[];
} YAP6__CORE__Blob;

/*
 * The YAP6__CORE__Container is a type of object that contains other object inside 
 * it. Its dispatcher must be a YAP6__CORE__ContainerDispatcher. 
 */
struct YAP6__CORE__Scalar {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__ScalarDispatcher* dispatcher;
  YAP6__CORE__Value* cell;
};

/*
 * The YAP6__CORE__ContainerDispatcher is a type of container that
 * also implements the FETCH and STORE methods.
 */
struct YAP6__CORE__ScalarDispatcher {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  YAP6__CORE__Value* (*APPLY)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Capture* arguments,
                               YAP6__CORE__Value* wants);
  void               (*DESTR)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value);
  int                (*COMPR)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* other);
  // REFCOUNT: the return of this method is counted as a refcount
  YAP6__CORE__Value* (*FETCH)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* wants);
  // REFCOUNT: the return of this method is counted as a refcount
  YAP6__CORE__Value* (*STORE)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* newvalue);
};


typedef struct YAP6__CORE__ListDispatcher {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  YAP6__CORE__Value* (*APPLY)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Capture* arguments,
                               YAP6__CORE__Value* wants);
  void               (*DESTR)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value);
  int                (*COMPR)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* other);
  // Lookup returns the value or the proxy value
  // REFCOUNT: the return of this method is counted as a refcount
  YAP6__CORE__Scalar* (*LOOKP)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__int* index);
  // Exists doesn't vivifies and returns only if it exists,
  // else return NULL
  // REFCOUNT: the return of this method is counted as a refcount
  YAP6__CORE__Scalar* (*EXIST)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__int* index);
  // Delete removes the key and returns it.
  // REFCOUNT: the return of this method is counted as a refcount
  YAP6__CORE__Scalar* (*DELET)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__int* index);
} YAP6__CORE__ListDispatcher;

typedef struct YAP6__CORE__List {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__ListDispatcher* dispatcher;
  int length;
  YAP6__CORE__Value** items;
} YAP6__CORE__List;

typedef struct YAP6__CORE__List__ProxyScalar {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__ScalarDispatcher* dispatcher;
  YAP6__CORE__Value* cell;
  YAP6__CORE__List* owner;
  YAP6__CORE__int* index;
} YAP6__CORE__List__ProxyScalar;

/* list support */
extern YAP6__CORE__ListDispatcher* yap6_const_list_dispatcher;
extern YAP6__CORE__ScalarDispatcher* yap6_const_list_proxyscalar_dispatcher;
extern void yap6_list_dispatcher_init();
extern YAP6__CORE__List* yap6_list_create();

typedef struct YAP6__CORE__Pair {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__ListDispatcher* dispatcher;
  YAP6__CORE__Value* key;
  YAP6__CORE__Value* value;
} YAP6__CORE__Pair;

typedef struct YAP6__CORE__HashDispatcher {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  YAP6__CORE__Value* (*APPLY)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Capture* arguments,
                               YAP6__CORE__Value* wants);
  void               (*DESTR)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value);
  int                (*COMPR)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* other);
  // REFCOUNT: the return of this method is counted as a refcount
  // Lookup returns the value or the proxy value
  YAP6__CORE__Scalar* (*LOOKP)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* key);
  // Exists doesn't vivifies and returns only if it exists,
  // else return NULL
  // REFCOUNT: the return of this method is counted as a refcount
  YAP6__CORE__Scalar* (*EXIST)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* key);
  // Delete removes the key and returns it.
  // REFCOUNT: the return of this method is counted as a refcount
  YAP6__CORE__Scalar* (*DELET)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* key);
} YAP6__CORE__HashDispatcher;

typedef struct YAP6__CORE__Hash {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__HashDispatcher* dispatcher;
  int length;
  YAP6__CORE__Pair** pairs;
} YAP6__CORE__Hash;

/* CaptureDispatcher is the low-level implementation of arguments
 * capture.
 */
struct YAP6__CORE__CaptureDispatcher {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  YAP6__CORE__Value* (*APPLY)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Capture* arguments,
                               YAP6__CORE__Value* wants);
  void               (*DESTR)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value);
  int                (*COMPR)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* other);
  // REFCOUNT: the return of this method is counted as a refcount
  // Gets a positional value
  YAP6__CORE__Scalar* (*LOOKP)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__int* index,
                               YAP6__CORE__Value* wants);
  // REFCOUNT: the return of this method is counted as a refcount
  // Gets a named value
  YAP6__CORE__Scalar* (*LOOKN)(YAP6__CORE__Dispatcher* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* key,
                               YAP6__CORE__Value* wants);
};

/* Capture is the format for the arguments, for now it'll remain
 * low-level. It contains the invocant, and the arguments, both
 * positional and named.
 */
struct YAP6__CORE__Capture {
  pthread_rwlock_t* rwlock; int ref_cnt;
  YAP6__CORE__CaptureDispatcher* dispatcher;
  YAP6__CORE__Value* invocant;
  YAP6__CORE__List* positional;
  YAP6__CORE__Hash* named;
};

// ident_dispatcher
extern YAP6__CORE__Dispatcher* yap6_const_ident_dispatcher;
extern void yap6_ident_dispatcher_init();

// const values
extern YAP6__CORE__Value* yap6_const_undef;
extern YAP6__CORE__Value* yap6_const_true;
extern YAP6__CORE__Value* yap6_const_false;
extern void yap6_const_init();

// basic object management
/* This function is the place from where every allocation should
   happen. For now, it just mallocs with zeros, set refcnt to 1 and
   initialize the rwlock. But it is subject to change, so, keep
   calling it. */
extern YAP6__CORE__Value* yap6_value_alloc(int size);
/* This function increments the reference count of a value, it
   should be called whenever the value is referenced by another
   value */
extern void yap6_value_refcnt_inc(YAP6__CORE__Value* value);
/* This functions decrements the reference count of a value, it should
   be called whenever one reference to this value is destroied. It
   will call DESTR in the dispatcher and free() the pointer when
   appropriate. */
extern void yap6_value_refcnt_dec(YAP6__CORE__Value* value);
/* This functions synchronizes the access to this value. It should be
   called whenever some pointer in the low-level details (some
   non-core-value member of the struct) will be accessed. */
extern void yap6_value_rdlock(YAP6__CORE__Value* value);
extern void yap6_value_wrlock(YAP6__CORE__Value* value);
extern void yap6_value_unlock(YAP6__CORE__Value* value);

/* Dispatching mechanism... This can be rewritten in the future,
   but for now, it's as simple as it gets */
#define YAP6_APPLY(value,arguments,wants) (value->dispatcher?\
                                           value->dispatcher->APPLY(\
                                              value->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Capture*)arguments,\
                                              (YAP6__CORE__Value*)wants\
                                           ):\
                                           ((YAP6__CORE__ListDispatcher*)value)->APPLY(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Capture*)arguments,\
                                              (YAP6__CORE__Value*)wants))

/* Dispatching mechanism... This can be rewritten in the future,
   but for now, it's as simple as it gets */
#define YAP6_LIST_LOOKP(value,index) (value->dispatcher?\
                                           value->dispatcher->LOOKP(\
                                              ((YAP6__CORE__ListDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index\
                                           ):\
                                           ((YAP6__CORE__ListDispatcher*)value)->LOOKP(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index))

/* Dispatching mechanism... This can be rewritten in the future,
   but for now, it's as simple as it gets */
#define YAP6_LIST_EXIST(value,index) (value->dispatcher?\
                                           value->dispatcher->EXIST(\
                                              ((YAP6__CORE__ListDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index\
                                           ):\
                                           ((YAP6__CORE__ListDispatcher*)value)->EXIST(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index))

/* Dispatching mechanism... This can be rewritten in the future,
   but for now, it's as simple as it gets */
#define YAP6_SCALAR_FETCH(value,wants) (value->dispatcher?\
                                           ((YAP6__CORE__ScalarDispatcher*)value->dispatcher)->FETCH(\
                                              ((YAP6__CORE__Dispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)wants\
                                           ):\
                                           ((YAP6__CORE__ScalarDispatcher*)value)->FETCH(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)wants))

/* Dispatching mechanism... This can be rewritten in the future,
   but for now, it's as simple as it gets */
#define YAP6_SCALAR_STORE(value,newvalue) (value->dispatcher?\
                                           ((YAP6__CORE__ScalarDispatcher*)value->dispatcher)->STORE(\
                                              ((YAP6__CORE__ScalarDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)newvalue\
                                           ):\
                                           ((YAP6__CORE__ScalarDispatcher*)value)->STORE(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)newvalue))
#endif
