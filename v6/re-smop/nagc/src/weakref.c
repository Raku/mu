#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>

typedef struct SMOP__NAGC__WeakRef {
  SMOP__NAGC__Object__BASE
  SMOP__NAGC__Object* ref;
} SMOP__NAGC__WeakRef;

SMOP__NAGC__ResponderInterface* SMOP__NAGC__WeakRef__RI;
static SMOP__Object* idconst_ref;
static SMOP__Object* idconst_redispatch;

// this creates a new weakref pointing to value
SMOP__Object* smop_nagc_weakref_create(SMOP__NAGC__Object* value) {
  SMOP__NAGC__WeakRef wr = smop_nagc_alloc(sizeof(SMOP__NAGC__WeakRef));
  wr->RI = (SMOP__ResponderInterface*)SMOP__NAGC__WeakRef__RI;
  wr->ref = value;
  
  smop_nagc_wrlock(value);
  void** item = calloc(2,sizeof(void*));
  item[0] = value;
  item[1] = value->weakrefs;
  value->weakrefs = item;
  smop_nagc_unlock(value);
}

// this de-register this weakref from the original value
// when the weakref is being destroyed
void smop_nagc_weakref_dereg(SMOP__NAGC__Object* value) {
  smop_nagc_wrlock(value);
  SMOP__NAGC__Object* original = value->ref;
  value->ref = SMOP__NATIVE__bool_false;
  smop_nagc_unlock(value);

  smop_nagc_wrlock(original);
  void** current = original->weakrefs;
  while (current) {
    if (current[0] == value) {
      void** next = (void**)current[1];
      current[0] = next[0];
      current[1] = next[1];
      break;
    }
    current = (void**)current[1];
  }
  smop_nagc_unlock(original);
}

// this is used to notify that the original value no longer exists
void smop_nagc_weakref_lostref(SMOP__NAGC__Object* value) {
  smop_nagc_wrlock(value);
  value->ref = SMOP__NATIVE__bool_false;
  smop_nagc_unlock(value);
}

// this is used during destruction, to cleanup the weakreferences
void smop_nagc_weakref_cleanup(SMOP__NAGC__Object* original) {
  smop_nagc_wrlock(original);
  void** current = original->weakrefs;
  while (current) {
    SMOP__NAGC__Object* obj = (SMOP__NAGC__Object*)current[0];
    smop_nagc_weakref_lostref(obj);
    current = (void**)current[1];
  }
  smop_nagc_unlock(original);
}

void weakref_destroy(SMOP__Object* interpreter, SMOP__Object* obj) {
  smop_nagc_weakref_dereg(obj);
}

void smop_nagc_weakref_init() {
  SMOP__NAGC__WeakRef__RI = calloc(1,sizeof(SMOP__NAGC__ResponderInterface));
  SMOP__NAGC__WeakRef__RI->REFERENCE = smop_nagc_reference;
  SMOP__NAGC__WeakRef__RI->RELEASE = smop_nagc_release;
  SMOP__NAGC__WeakRef__RI->WEAKREF = smop_nagc_weakref;
  SMOP__NAGC__WeakRef__RI->id = "Weak Reference";
  SMOP__NAGC__WeakRef__RI->DESTROYALL = weakref_destroy;
  SMOP__NAGC__WeakRef__RI->MESSAGE = smop_placeholder_message;
}

void smop_nagc_weakref_destr() {
  free(SMOP__NAGC__WeakRef__RI);
}
