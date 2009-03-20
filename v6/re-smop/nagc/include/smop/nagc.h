#ifndef SMOP_NAGC_H
#define SMOP_NAGC_H

#include <smop/base.h>
#include <pthread.h>
#define SMOP__NAGC__ResponderInterface__BASE     \
  SMOP__ResponderInterface__BASE                 \
  void (*DESTROYALL) (SMOP__Object* interpreter, \
                      SMOP__Object* object);

typedef struct SMOP__NAGC__ResponderInterface {
  SMOP__Object__BASE
  SMOP__NAGC__ResponderInterface__BASE
} SMOP__NAGC__ResponderInterface;

// weakrefs is handled privately...
#define SMOP__NAGC__Object__BASE      \
  SMOP__Object__BASE                  \
  int ref_cnt;                        \
  pthread_rwlock_t* rwlock;           \
  void** weakrefs;
  
typedef struct SMOP__NAGC__Object {
  SMOP__NAGC__Object__BASE
} SMOP__NAGC__Object;

typedef struct SMOP__NAGC__WeakRef {
  SMOP__NAGC__Object__BASE
  SMOP__NAGC__Object* ref;
  int lost;
} SMOP__NAGC__WeakRef;

extern SMOP__NAGC__ResponderInterface* SMOP__NAGC__WeakRef__RI;

extern SMOP__Object* smop_nagc_alloc(size_t size);


extern SMOP__Object* smop_nagc_reference(SMOP__Object* interpreter,
                                         SMOP__ResponderInterface* responder,
                                         SMOP__Object* obj); 
extern SMOP__Object* smop_nagc_release(SMOP__Object* interpreter,
                                       SMOP__ResponderInterface* responder,
                                       SMOP__Object* obj);
extern SMOP__Object* smop_nagc_weakref(SMOP__Object* interpreter,
                                       SMOP__ResponderInterface* responder,
                                       SMOP__Object* obj); 

extern void smop_nagc_rdlock(SMOP__NAGC__Object* obj);
extern void smop_nagc_wrlock(SMOP__NAGC__Object* obj);
extern void smop_nagc_unlock(SMOP__NAGC__Object* obj);

extern void smop_nagc_init();
extern void smop_nagc_destr();

#endif

