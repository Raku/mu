#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>

typedef struct SMOP__NAGC__WeakRef {
  SMOP__NAGC__Object__BASE
  int lost;
  SMOP__NAGC__Object* ref;
} SMOP__NAGC__WeakRef;

static SMOP__NAGC__ResponderInterface* ri;

// this creates a new weakref pointing to value
SMOP__Object* smop_nagc_weakref_create(SMOP__NAGC__Object* value) {
  SMOP__NAGC__WeakRef wr = smop_nagc_alloc(sizeof(SMOP__NAGC__WeakRef));
  wr->RI = (SMOP__ResponderInterface*)ri;
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
  value->lost = 1;
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
  SMOP__NAGC__Object* original = value->ref;
  value->ref = SMOP__NATIVE__bool_false;
  value->lost = 1;
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

SMOP__Object* weakref_message(SMOP__Object* interpreter,
                              SMOP__ResponderInterface* self,
                              SMOP__Object* identifier,
                              SMOP__Object* capture) {
  // hmm... we need capture here... 
  pritnf("unimplemented!\n");
  abort();
}

void smop_nagc_weakref_init() {
  ri = calloc(1,sizeof(SMOP__NAGC__ResponderInterface));
  ri->REFERENCE = smop_nagc_reference;
  ri->RELEASE = smop_nagc_release;
  ri->WEAKREF = smop_nagc_weakref;
  ri->id = "Weak Reference";
  ri->DESTROYALL = weakref_destroy;
  ri->MESSAGE = weakref_message;
}

void smop_nagc_weakref_destr() {
  free(ri);
}
