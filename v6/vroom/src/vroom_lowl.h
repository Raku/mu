#ifndef __VROOM_LOWL_H
#define __VROOM_LOWL_H



/* These are the core runtime routines of VROOM
 */
extern void vroom_init();
extern void vroom_destr();
extern void vroom_memory_init();
extern void vroom_memory_destr();
/* This function is the place from where every allocation should
   happen. For now, it just mallocs with zeros, set refcnt to 1 and
   initialize the rwlock. But it is subject to change, so, keep
   calling it. */
extern VROOM__CORE__Value* vroom_value_alloc(int size);
/* This function increments the reference count of a value, it
   should be called whenever the value is referenced by another
   value */
extern VROOM__CORE__Value* vroom_value_refcnt_inc(VROOM__CORE__Value* value);
/* This functions decrements the reference count of a value, it should
   be called whenever one reference to this value is destroied. It
   will call DESTR in the dispatcher and free() the pointer when
   appropriate. */
extern VROOM__CORE__Value* vroom_value_refcnt_dec(VROOM__CORE__Value* value);
/* This functions synchronizes the access to this value. It should be
   called whenever some pointer in the low-level details (some
   non-core-value member of the struct) will be accessed. */
extern void vroom_value_rdlock(VROOM__CORE__Value* value);
extern void vroom_value_wrlock(VROOM__CORE__Value* value);
extern void vroom_value_unlock(VROOM__CORE__Value* value);


struct VROOM__CORE__string; typedef struct VROOM__CORE__string VROOM__CORE__string;
struct VROOM__CORE__num; typedef struct VROOM__CORE__num VROOM__CORE__num;
struct VROOM__CORE__int; typedef struct VROOM__CORE__int VROOM__CORE__int;
struct VROOM__CORE__bytes; typedef struct VROOM__CORE__bytes VROOM__CORE__bytes;
struct VROOM__CORE__Scalar; typedef struct VROOM__CORE__Scalar VROOM__CORE__Scalar;
struct VROOM__CORE__List; typedef struct VROOM__CORE__List VROOM__CORE__List;
struct VROOM__CORE__Pair; typedef struct VROOM__CORE__Pair VROOM__CORE__Pair;
struct VROOM__CORE__Hash; typedef struct VROOM__CORE__Hash VROOM__CORE__Hash;
struct VROOM__CORE__Capture; typedef struct VROOM__CORE__Capture VROOM__CORE__Capture;
struct VROOM__CORE__ScalarDispatcher; typedef struct VROOM__CORE__ScalarDispatcher VROOM__CORE__ScalarDispatcher;
struct VROOM__CORE__ListDispatcher; typedef struct VROOM__CORE__ListDispatcher VROOM__CORE__ListDispatcher;
struct VROOM__CORE__PairDispatcher; typedef struct VROOM__CORE__PairDispatcher VROOM__CORE__PairDispatcher;
struct VROOM__CORE__HashDispatcher; typedef struct VROOM__CORE__HashDispatcher VROOM__CORE__HashDispatcher;
struct VROOM__CORE__CaptureDispatcher; typedef struct VROOM__CORE__CaptureDispatcher VROOM__CORE__CaptureDispatcher;

// ident_dispatcher
extern VROOM__CORE__Dispatcher* vroom_const_ident_dispatcher;
extern void vroom_ident_dispatcher_init();
extern void vroom_ident_dispatcher_which_init();
extern void vroom_ident_dispatcher_destr();

// const values
extern VROOM__CORE__Value* vroom_const_undef;
extern VROOM__CORE__Value* vroom_bool_false;
extern void vroom_const_init();
extern void vroom_const_destr();



#define VROOM__BASE__LOWL_Dispatcher                                   \
  VROOM__CORE__Value* (*CREATE)(VROOM__CORE__Dispatcher* self,          \
                               VROOM__CORE__List* arguments);          \
  void               (*DESTR)(VROOM__CORE__Dispatcher* self,           \
                               VROOM__CORE__Value* value);             \
  VROOM__CORE__string* (*STRNG)(VROOM__CORE__Dispatcher* self,          \
                               VROOM__CORE__Value* value);             \
  VROOM__CORE__num*   (*NUMBR)(VROOM__CORE__Dispatcher* self,           \
                               VROOM__CORE__Value* value);             \
  VROOM__CORE__Value* (*BOOLN)(VROOM__CORE__Dispatcher* self,           \
                               VROOM__CORE__Value* value);             \
  VROOM__CORE__Scalar* (*SCALAR)(VROOM__CORE__Dispatcher* self,         \
                               VROOM__CORE__Value* value);             \
  VROOM__CORE__List*  (*LIST) (VROOM__CORE__Dispatcher* self,           \
                               VROOM__CORE__Value* value);             \
  VROOM__CORE__Hash*  (*HASH) (VROOM__CORE__Dispatcher* self,           \
                               VROOM__CORE__Value* value);             \
  VROOM__CORE__bytes* (*WHICH)(VROOM__CORE__Dispatcher* self,           \
                               VROOM__CORE__Value* value);

struct VROOM__LOWL__Dispatcher {
  VROOM__BASE__Value
  VROOM__CORE__Dispatcher* dispatcher;
  VROOM__LOWL__Dispacher
}

/* int support */
struct VROOM__CORE__int {
  VROOM__BASE__Value
  VROOM__CORE__Dispatcher* dispatcher;
  int value;  
};

extern VROOM__CORE__Dispatcher* vroom_const_int_dispatcher;
extern void vroom_int_dispatcher_init();
extern void vroom_int_dispatcher_which_init();
extern VROOM__CORE__int* vroom_int_create(int initialvalue);
extern int vroom_int_lowlevel(VROOM__CORE__int* value);
extern void vroom_int_dispatcher_destr();

/* bytes support */
struct VROOM__CORE__bytes {
  VROOM__BASE__Value
  VROOM__CORE__Dispatcher* dispatcher;
  char* value;
  int size;
};

extern VROOM__CORE__Dispatcher* vroom_const_bytes_dispatcher;
extern void vroom_bytes_dispatcher_init();
extern VROOM__CORE__bytes* vroom_bytes_create(const char* initialvalue, int size);
extern char* vroom_bytes_lowlevel(VROOM__CORE__bytes* value, int* sizeret);
extern void vroom_bytes_dispatcher_destr();

struct VROOM__CORE__double {
  VROOM__BASE__Value
  VROOM__CORE__Dispatcher* dispatcher;
  double value;  
} VROOM__CORE__double;

struct VROOM__CORE__num {
  VROOM__BASE__Value
  VROOM__CORE__Dispatcher* dispatcher;
  enum { VROOM__CORE__num__INT, VROOM__CORE__num__DOUBLE,
         VROOM__CORE__num__BIGNUM } precision;
  int int_value;
  double double_value;
  char** bignum_value;
};

struct VROOM__CORE__string {
  VROOM__BASE__Value
  VROOM__CORE__Dispatcher* dispatcher;
  char encoding[16];
  int byte_length;
  char content[];
};

/*
 * The VROOM__CORE__Scalar is a type of object that contains other
 * object inside it.
 */
struct VROOM__CORE__Scalar {
  VROOM__BASE__Value
  VROOM__CORE__ScalarDispatcher* dispatcher;
  VROOM__CORE__Value* cell;
};

/* scalar support */
/*
 * The VROOM__CORE__ScalarDispatcher also implements the FETCH and
 * STORE methods.
 */
struct VROOM__CORE__ScalarDispatcher {
  VROOM__BASE__Value
  VROOM__CORE__Dispatcher* dispatcher;
  VROOM__BASE__Dispacher
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Value* (*FETCH)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value, 
                               VROOM__CORE__Value* wants);
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Value* (*STORE)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value, 
                               VROOM__CORE__Value* newvalue);
};

extern VROOM__CORE__ScalarDispatcher* vroom_const_scalar_dispatcher;
extern void vroom_scalar_dispatcher_init();
extern VROOM__CORE__Scalar* vroom_scalar_create(VROOM__CORE__Value* initialValue);
extern void vroom_scalar_dispatcher_destr();

struct VROOM__CORE__ListDispatcher {
  VROOM__BASE__Value
  VROOM__CORE__Dispatcher* dispatcher;
  VROOM__BASE__Dispacher
  // Lookup returns the value or the proxy value
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Scalar* (*LOOKP)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value, 
                               VROOM__CORE__int* index);
  // Exists doesn't vivifies and returns only if it exists,
  // else return NULL
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Scalar* (*EXIST)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value, 
                               VROOM__CORE__int* index);
  // Store without lookup returns the old value only if it exists,
  // else return NULL
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Value*  (*STORE)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value, 
                               VROOM__CORE__int* index,
                               VROOM__CORE__Value* newvalue);
  // Delete removes the key and returns it.
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Scalar* (*DELET)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value, 
                               VROOM__CORE__int* index);
  // returns the number of elements
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__int*    (*ELEMS)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value);
};

/* list support */
struct VROOM__CORE__List {
  VROOM__BASE__Value
  VROOM__CORE__ListDispatcher* dispatcher;
  int length;
  VROOM__CORE__Value** items;
};

extern VROOM__CORE__ListDispatcher* vroom_const_list_dispatcher;
extern void vroom_list_dispatcher_init();
extern VROOM__CORE__List* vroom_list_create();
extern void vroom_list_dispatcher_destr();

/* pair support */
/*
 * The VROOM__CORE__PairDispatcher is a type of container that
 * also implements the GTKEY GTVAL STVAL methods.
 */
struct VROOM__CORE__PairDispatcher {
  VROOM__BASE__Value
  VROOM__CORE__Dispatcher* dispatcher;
  VROOM__BASE__Dispacher
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Value* (*GTKEY)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value);
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Value* (*GTVAL)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value);
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Value* (*STVAL)(VROOM__CORE__Dispatcher* self,
                              VROOM__CORE__Value* value,
                              VROOM__CORE__Value* newval);
};

struct VROOM__CORE__Pair {
  VROOM__BASE__Value
  VROOM__CORE__PairDispatcher* dispatcher;
  VROOM__CORE__Value* key;
  VROOM__CORE__Value* value;
};

extern VROOM__CORE__PairDispatcher* vroom_const_pair_dispatcher;
extern void vroom_pair_dispatcher_init();
extern VROOM__CORE__Pair* vroom_pair_create(VROOM__CORE__Value* key, VROOM__CORE__Value* value);
extern void vroom_pair_dispatcher_destr();

struct VROOM__CORE__HashDispatcher {
  VROOM__BASE__Value
  VROOM__CORE__Dispatcher* dispatcher;
  VROOM__BASE__Dispacher
  // REFCOUNT: the return of this method is counted as a refcount
  // Lookup returns the value or the proxy value
  VROOM__CORE__Scalar* (*LOOKP)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value, 
                               VROOM__CORE__Value* key);
  // Exists doesn't vivifies and returns only if it exists,
  // else return NULL
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Scalar* (*EXIST)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value, 
                               VROOM__CORE__Value* key);
  // Store without lookup returns the old value only if it exists,
  // else return NULL
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Value*  (*STORE)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value, 
                               VROOM__CORE__Value* key,
                               VROOM__CORE__Value* newvalue);
  // Delete removes the key and returns it.
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__Scalar* (*DELET)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value, 
                               VROOM__CORE__Value* key);
  // returns the number of elements
  // REFCOUNT: the return of this method is counted as a refcount
  VROOM__CORE__int*    (*ELEMS)(VROOM__CORE__Dispatcher* self,
                               VROOM__CORE__Value* value);

};

struct VROOM__CORE__Hash {
  VROOM__BASE__Value
  VROOM__CORE__HashDispatcher* dispatcher;
  int length;
  VROOM__CORE__Pair** pairs;
};


// the capture object doesn't need any custom methods, as scalar it
// returns the invocant, as list the positional and as hash the named
// arguments.
struct VROOM__CORE__Capture {
  VROOM__BASE__Value
  VROOM__CORE__Dispatcher* dispatcher;
  VROOM__CORE__Value* invocant;
  VROOM__CORE__Hash* named;
  VROOM__CORE__List* positional;
};

#include "vroom_lowl_macros.h"

#endif
