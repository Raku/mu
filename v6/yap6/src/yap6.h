
#ifndef __YAP6_H
#define __YAP6_H

#include <pthread.h>

// forward declarations
struct YAP6__CORE__Value; typedef struct YAP6__CORE__Value YAP6__CORE__Value;
struct YAP6__CORE__Scalar; typedef struct YAP6__CORE__Scalar YAP6__CORE__Scalar;
struct YAP6__CORE__Dispatcher; typedef struct YAP6__CORE__Dispatcher YAP6__CORE__Dispatcher;
struct YAP6__CORE__ScalarDispatcher; typedef struct YAP6__CORE__ScalarDispatcher YAP6__CORE__ScalarDispatcher;

/*
 * The YAP6__CORE__Oject struct represents any object in the YAP6 runtime.
 * The data of this object should be opaque for the users, the only
 * one that should know about it is the dispatcher. Every object must
 * have a dispatcher. If it doesn't, it is considered itself as one.
 */
struct YAP6__CORE__Value {
  pthread_mutex_t* mutex; int ref_cnt;
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
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  YAP6__CORE__Value* (*APPLY)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments,
                               YAP6__CORE__Value* wants);
  YAP6__CORE__Value* (*DESTR)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments,
                               YAP6__CORE__Value* wants);
  int                (*COMPR)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments);
};

typedef struct YAP6__CORE__int {
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  int value;  
} YAP6__CORE__int;

typedef struct YAP6__CORE__double {
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  double value;  
} YAP6__CORE__double;

typedef struct YAP6__CORE__string {
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  char encoding[16];
  int byte_length;
  char content[];
} YAP6__CORE__String;

typedef struct YAP6__CORE__blob {
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  int byte_length;
  char content[];
} YAP6__CORE__Blob;

/*
 * The YAP6__CORE__Container is a type of object that contains other object inside 
 * it. Its dispatcher must be a YAP6__CORE__ContainerDispatcher. 
 */
struct YAP6__CORE__Scalar {
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__ScalarDispatcher* dispatcher;
  YAP6__CORE__Value* cell;
};

/*
 * The YAP6__CORE__ContainerDispatcher is a type of container that
 * also implements the FETCH and STORE methods.
 */
struct YAP6__CORE__ScalarDispatcher {
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  YAP6__CORE__Value* (*APPLY)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments,
                               YAP6__CORE__Value* wants);
  YAP6__CORE__Value* (*DESTR)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments,
                               YAP6__CORE__Value* wants);
  int                (*COMPR)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments);
  YAP6__CORE__Value* (*FETCH)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments,
                               YAP6__CORE__Value* wants);
  YAP6__CORE__Value* (*STORE)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments,
                               YAP6__CORE__Value* wants);
};


typedef struct YAP6__CORE__ListDispatcher {
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  YAP6__CORE__Value* (*APPLY)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments,
                               YAP6__CORE__Value* wants);
  YAP6__CORE__Value* (*DESTR)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments,
                               YAP6__CORE__Value* wants);
  int                (*COMPR)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments);
  // Lookup returns the value or the proxy value
  YAP6__CORE__Value* (*LOOKP)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__int* index,
                               YAP6__CORE__Value* wants);
  // Exists doesn't vivifies and returns only if it exists,
  // else return NULL
  YAP6__CORE__Value* (*EXIST)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__int* index,
                               YAP6__CORE__Value* wants);
  // Delete removes the key and returns it.
  YAP6__CORE__Value* (*DELET)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__int* index,
                               YAP6__CORE__Value* wants);
} YAP6__CORE__ListDispatcher;

typedef struct YAP6__CORE__List {
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__ListDispatcher* dispatcher;
  int length;
  YAP6__CORE__Value** items;
} YAP6__CORE__List;

typedef struct YAP6__CORE__Pair {
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__ListDispatcher* dispatcher;
  YAP6__CORE__Value* key;
  YAP6__CORE__Value* value;
} YAP6__CORE__Pair;

typedef struct YAP6__CORE__HashDispatcher {
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__Dispatcher* dispatcher;
  YAP6__CORE__Value* (*APPLY)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments,
                               YAP6__CORE__Value* wants);
  YAP6__CORE__Value* (*DESTR)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments,
                               YAP6__CORE__Value* wants);
  int                (*COMPR)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* arguments);
  // Lookup returns the value or the proxy value
  YAP6__CORE__Value* (*LOOKP)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* key,
                               YAP6__CORE__Value* wants);
  // Exists doesn't vivifies and returns only if it exists,
  // else return NULL
  YAP6__CORE__Value* (*EXIST)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* key,
                               YAP6__CORE__Value* wants);
  // Delete removes the key and returns it.
  YAP6__CORE__Value* (*DELET)(YAP6__CORE__Value* self,
                               YAP6__CORE__Value* value, 
                               YAP6__CORE__Value* key,
                               YAP6__CORE__Value* wants);
} YAP6__CORE__HashDispatcher;

typedef struct YAP6__CORE__Hash {
  pthread_mutex_t* mutex; int ref_cnt;
  YAP6__CORE__HashDispatcher* dispatcher;
  int length;
  YAP6__CORE__Pair** pairs;
} YAP6__CORE__Hash;


// ident_dispatcher
extern YAP6__CORE__Dispatcher* yap6_const_ident_dispatcher;
extern void yap6_ident_dispatcher_init();

// const values
extern YAP6__CORE__Value* yap6_const_undef;
extern YAP6__CORE__Value* yap6_const_true;
extern YAP6__CORE__Value* yap6_const_false;
extern void yap6_const_init();

#endif
