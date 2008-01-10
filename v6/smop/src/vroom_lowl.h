#ifndef __SMOP_LOWL_H
#define __SMOP_LOWL_H



/* These are the core runtime routines of SMOP
 */
extern void smop_init();
extern void smop_destr();
extern void smop_memory_init();
extern void smop_memory_destr();
/* This function is the place from where every allocation should
   happen. For now, it just mallocs with zeros, set refcnt to 1 and
   initialize the rwlock. But it is subject to change, so, keep
   calling it. */
extern SMOP__CORE__Value* smop_value_alloc(int size);
/* This function increments the reference count of a value, it
   should be called whenever the value is referenced by another
   value */
extern SMOP__CORE__Value* smop_value_refcnt_inc(SMOP__CORE__Value* value);
/* This functions decrements the reference count of a value, it should
   be called whenever one reference to this value is destroied. It
   will call DESTR in the dispatcher and free() the pointer when
   appropriate. */
extern SMOP__CORE__Value* smop_value_refcnt_dec(SMOP__CORE__Value* value);
/* This functions synchronizes the access to this value. It should be
   called whenever some pointer in the low-level details (some
   non-core-value member of the struct) will be accessed. */
extern void smop_value_rdlock(SMOP__CORE__Value* value);
extern void smop_value_wrlock(SMOP__CORE__Value* value);
extern void smop_value_unlock(SMOP__CORE__Value* value);


struct SMOP__CORE__string; typedef struct SMOP__CORE__string SMOP__CORE__string;
struct SMOP__CORE__num; typedef struct SMOP__CORE__num SMOP__CORE__num;
struct SMOP__CORE__int; typedef struct SMOP__CORE__int SMOP__CORE__int;
struct SMOP__CORE__bytes; typedef struct SMOP__CORE__bytes SMOP__CORE__bytes;
struct SMOP__CORE__Scalar; typedef struct SMOP__CORE__Scalar SMOP__CORE__Scalar;
struct SMOP__CORE__List; typedef struct SMOP__CORE__List SMOP__CORE__List;
struct SMOP__CORE__Pair; typedef struct SMOP__CORE__Pair SMOP__CORE__Pair;
struct SMOP__CORE__Hash; typedef struct SMOP__CORE__Hash SMOP__CORE__Hash;
struct SMOP__CORE__Capture; typedef struct SMOP__CORE__Capture SMOP__CORE__Capture;
struct SMOP__CORE__ScalarDispatcher; typedef struct SMOP__CORE__ScalarDispatcher SMOP__CORE__ScalarDispatcher;
struct SMOP__CORE__ListDispatcher; typedef struct SMOP__CORE__ListDispatcher SMOP__CORE__ListDispatcher;
struct SMOP__CORE__PairDispatcher; typedef struct SMOP__CORE__PairDispatcher SMOP__CORE__PairDispatcher;
struct SMOP__CORE__HashDispatcher; typedef struct SMOP__CORE__HashDispatcher SMOP__CORE__HashDispatcher;
struct SMOP__CORE__CaptureDispatcher; typedef struct SMOP__CORE__CaptureDispatcher SMOP__CORE__CaptureDispatcher;

// ident_dispatcher
extern SMOP__CORE__Dispatcher* smop_const_ident_dispatcher;
extern void smop_ident_dispatcher_init();
extern void smop_ident_dispatcher_which_init();
extern void smop_ident_dispatcher_destr();

// const values
extern SMOP__CORE__Value* smop_const_undef;
extern SMOP__CORE__Value* smop_bool_false;
extern void smop_const_init();
extern void smop_const_destr();



#define SMOP__BASE__LOWL_Dispatcher                                   \
  SMOP__CORE__Value* (*CREATE)(SMOP__CORE__Dispatcher* self,          \
                               SMOP__CORE__List* arguments);          \
  void               (*DESTR)(SMOP__CORE__Dispatcher* self,           \
                               SMOP__CORE__Value* value);             \
  SMOP__CORE__string* (*STRNG)(SMOP__CORE__Dispatcher* self,          \
                               SMOP__CORE__Value* value);             \
  SMOP__CORE__num*   (*NUMBR)(SMOP__CORE__Dispatcher* self,           \
                               SMOP__CORE__Value* value);             \
  SMOP__CORE__Value* (*BOOLN)(SMOP__CORE__Dispatcher* self,           \
                               SMOP__CORE__Value* value);             \
  SMOP__CORE__Scalar* (*SCALAR)(SMOP__CORE__Dispatcher* self,         \
                               SMOP__CORE__Value* value);             \
  SMOP__CORE__List*  (*LIST) (SMOP__CORE__Dispatcher* self,           \
                               SMOP__CORE__Value* value);             \
  SMOP__CORE__Hash*  (*HASH) (SMOP__CORE__Dispatcher* self,           \
                               SMOP__CORE__Value* value);             \
  SMOP__CORE__bytes* (*WHICH)(SMOP__CORE__Dispatcher* self,           \
                               SMOP__CORE__Value* value);

struct SMOP__LOWL__Dispatcher {
  SMOP__BASE__Value
  SMOP__CORE__Dispatcher* dispatcher;
  SMOP__LOWL__Dispacher
}

/* int support */
struct SMOP__CORE__int {
  SMOP__BASE__Value
  SMOP__CORE__Dispatcher* dispatcher;
  int value;  
};

extern SMOP__CORE__Dispatcher* smop_const_int_dispatcher;
extern void smop_int_dispatcher_init();
extern void smop_int_dispatcher_which_init();
extern SMOP__CORE__int* smop_int_create(int initialvalue);
extern int smop_int_lowlevel(SMOP__CORE__int* value);
extern void smop_int_dispatcher_destr();

/* bytes support */
struct SMOP__CORE__bytes {
  SMOP__BASE__Value
  SMOP__CORE__Dispatcher* dispatcher;
  char* value;
  int size;
};

extern SMOP__CORE__Dispatcher* smop_const_bytes_dispatcher;
extern void smop_bytes_dispatcher_init();
extern SMOP__CORE__bytes* smop_bytes_create(const char* initialvalue, int size);
extern char* smop_bytes_lowlevel(SMOP__CORE__bytes* value, int* sizeret);
extern void smop_bytes_dispatcher_destr();

struct SMOP__CORE__double {
  SMOP__BASE__Value
  SMOP__CORE__Dispatcher* dispatcher;
  double value;  
} SMOP__CORE__double;

struct SMOP__CORE__num {
  SMOP__BASE__Value
  SMOP__CORE__Dispatcher* dispatcher;
  enum { SMOP__CORE__num__INT, SMOP__CORE__num__DOUBLE,
         SMOP__CORE__num__BIGNUM } precision;
  int int_value;
  double double_value;
  char** bignum_value;
};

struct SMOP__CORE__string {
  SMOP__BASE__Value
  SMOP__CORE__Dispatcher* dispatcher;
  char encoding[16];
  int byte_length;
  char content[];
};

/*
 * The SMOP__CORE__Scalar is a type of object that contains other
 * object inside it.
 */
struct SMOP__CORE__Scalar {
  SMOP__BASE__Value
  SMOP__CORE__ScalarDispatcher* dispatcher;
  SMOP__CORE__Value* cell;
};

/* scalar support */
/*
 * The SMOP__CORE__ScalarDispatcher also implements the FETCH and
 * STORE methods.
 */
struct SMOP__CORE__ScalarDispatcher {
  SMOP__BASE__Value
  SMOP__CORE__Dispatcher* dispatcher;
  SMOP__BASE__Dispacher
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Value* (*FETCH)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value, 
                               SMOP__CORE__Value* wants);
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Value* (*STORE)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value, 
                               SMOP__CORE__Value* newvalue);
};

extern SMOP__CORE__ScalarDispatcher* smop_const_scalar_dispatcher;
extern void smop_scalar_dispatcher_init();
extern SMOP__CORE__Scalar* smop_scalar_create(SMOP__CORE__Value* initialValue);
extern void smop_scalar_dispatcher_destr();

struct SMOP__CORE__ListDispatcher {
  SMOP__BASE__Value
  SMOP__CORE__Dispatcher* dispatcher;
  SMOP__BASE__Dispacher
  // Lookup returns the value or the proxy value
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Scalar* (*LOOKP)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value, 
                               SMOP__CORE__int* index);
  // Exists doesn't vivifies and returns only if it exists,
  // else return NULL
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Scalar* (*EXIST)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value, 
                               SMOP__CORE__int* index);
  // Store without lookup returns the old value only if it exists,
  // else return NULL
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Value*  (*STORE)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value, 
                               SMOP__CORE__int* index,
                               SMOP__CORE__Value* newvalue);
  // Delete removes the key and returns it.
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Scalar* (*DELET)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value, 
                               SMOP__CORE__int* index);
  // returns the number of elements
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__int*    (*ELEMS)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value);
};

/* list support */
struct SMOP__CORE__List {
  SMOP__BASE__Value
  SMOP__CORE__ListDispatcher* dispatcher;
  int length;
  SMOP__CORE__Value** items;
};

extern SMOP__CORE__ListDispatcher* smop_const_list_dispatcher;
extern void smop_list_dispatcher_init();
extern SMOP__CORE__List* smop_list_create();
extern void smop_list_dispatcher_destr();

/* pair support */
/*
 * The SMOP__CORE__PairDispatcher is a type of container that
 * also implements the GTKEY GTVAL STVAL methods.
 */
struct SMOP__CORE__PairDispatcher {
  SMOP__BASE__Value
  SMOP__CORE__Dispatcher* dispatcher;
  SMOP__BASE__Dispacher
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Value* (*GTKEY)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value);
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Value* (*GTVAL)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value);
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Value* (*STVAL)(SMOP__CORE__Dispatcher* self,
                              SMOP__CORE__Value* value,
                              SMOP__CORE__Value* newval);
};

struct SMOP__CORE__Pair {
  SMOP__BASE__Value
  SMOP__CORE__PairDispatcher* dispatcher;
  SMOP__CORE__Value* key;
  SMOP__CORE__Value* value;
};

extern SMOP__CORE__PairDispatcher* smop_const_pair_dispatcher;
extern void smop_pair_dispatcher_init();
extern SMOP__CORE__Pair* smop_pair_create(SMOP__CORE__Value* key, SMOP__CORE__Value* value);
extern void smop_pair_dispatcher_destr();

struct SMOP__CORE__HashDispatcher {
  SMOP__BASE__Value
  SMOP__CORE__Dispatcher* dispatcher;
  SMOP__BASE__Dispacher
  // REFCOUNT: the return of this method is counted as a refcount
  // Lookup returns the value or the proxy value
  SMOP__CORE__Scalar* (*LOOKP)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value, 
                               SMOP__CORE__Value* key);
  // Exists doesn't vivifies and returns only if it exists,
  // else return NULL
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Scalar* (*EXIST)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value, 
                               SMOP__CORE__Value* key);
  // Store without lookup returns the old value only if it exists,
  // else return NULL
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Value*  (*STORE)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value, 
                               SMOP__CORE__Value* key,
                               SMOP__CORE__Value* newvalue);
  // Delete removes the key and returns it.
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__Scalar* (*DELET)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value, 
                               SMOP__CORE__Value* key);
  // returns the number of elements
  // REFCOUNT: the return of this method is counted as a refcount
  SMOP__CORE__int*    (*ELEMS)(SMOP__CORE__Dispatcher* self,
                               SMOP__CORE__Value* value);

};

struct SMOP__CORE__Hash {
  SMOP__BASE__Value
  SMOP__CORE__HashDispatcher* dispatcher;
  int length;
  SMOP__CORE__Pair** pairs;
};


// the capture object doesn't need any custom methods, as scalar it
// returns the invocant, as list the positional and as hash the named
// arguments.
struct SMOP__CORE__Capture {
  SMOP__BASE__Value
  SMOP__CORE__Dispatcher* dispatcher;
  SMOP__CORE__Value* invocant;
  SMOP__CORE__Hash* named;
  SMOP__CORE__List* positional;
};

#include "smop_lowl_macros.h"

#endif
