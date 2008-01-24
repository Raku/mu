#include <smop.h>
#include <smop_lowlevel.h>

SMOP__Object* SMOP__NATIVE__int;

typedef struct smop_native_int_struct {
  SMOP__Object__BASE
  int intvalue;
}

static SMOP__Object* int_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  // todo
  return self;
}

static SMOP__Object* int_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if (responder != obj) {
    smop_lowlevel_refcnt_inc(interpreter, responder, obj);
  }
  return obj;
}

static SMOP__Object* int_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if (responder != obj) {
    smop_lowlevel_refcnt_dec(interpreter, responder, obj);
  }
  return obj;
}

void smop_native_int_init() {
  SMOP__NATIVE__int = calloc(sizeof(SMOP__ResponderInterface));
  SMOP__NATIVE__int->MESSAGE = int_message;
  SMOP__NATIVE__int->REFERENCE = int_reference;
  SMOP__NATIVE__int->RELEASE = int_release;
}

void smop_native_int_destr() {
  free(SMOP__SLIME__CurrentFrame);
}

SMOP__Object* SMOP__NATIVE__int_create(int value) {
  SMOP__Object* ret = smop_lowlevel_alloc(sizeof(smop_native_int_struct));
  ret->RI = SMOP__NATIVE__int;
  ((smop_native_int_struct*)ret)->intvalue = value;
  return ret;
}

int SMOP__NATIVE__int_fetch(SMOP__Object* value) {
  smop_lowlevel_rdlock(value);
  int v = ((smop_native_int_struct*)ret)->intvalue;
  smop_lowlevel_unlock(value);
  return v;
}
