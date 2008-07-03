
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <smop.h>
#include <smop_oo.h>
#include <smop_lowlevel.h>

typedef struct SMOP__p6opaque_LOWL_A {
  int count;
  SMOP__Object** array;
} SMOP__p6opaque_LOWL_A;

typedef struct SMOP__p6opaque_LOWL_P {
  SMOP__Object* key;
  SMOP__Object* value;
} SMOP__p6opaque_LOWL_P;

typedef struct SMOP__p6opaque_LOWL_H {
  int count;
  SMOP__p6opaque_LOWL_P* pairs;
} SMOP__p6opaque_LOWL_H;

typedef struct SMOP__p6opaque_LOWL_PoA {
  SMOP__Object* key;
  SMOP__p6opaque_LOWL_A value;
} SMOP__p6opaque_LOWL_PoA;

typedef struct SMOP__p6opaque_LOWL_PoH {
  SMOP__Object* key;
  SMOP__p6opaque_LOWL_H value;
} SMOP__p6opaque_LOWL_PoH;

typedef struct SMOP__p6opaque_LOWL_HoA {
  int count;
  SMOP__p6opaque_LOWL_PoA* pairs;
} SMOP__p6opaque_LOWL_HoA;

typedef struct SMOP__p6opaque_LOWL_HoH {
  int count;
  SMOP__p6opaque_LOWL_PoH* pairs;
} SMOP__p6opaque_LOWL_HoH;

typedef struct SMOP__p6opaque_LOWL_metadata {
  SMOP__Object* how;
  SMOP__Object* package;
  SMOP__p6opaque_LOWL_A isa;
  SMOP__p6opaque_LOWL_A does;
  SMOP__p6opaque_LOWL_H class_storage_optimal;
  SMOP__p6opaque_LOWL_H class_storage;
  SMOP__p6opaque_LOWL_H attributes_optimal;
  SMOP__p6opaque_LOWL_H attributes;
  SMOP__p6opaque_LOWL_HoA methods_optimal;
  SMOP__p6opaque_LOWL_HoA methods;
} SMOP__p6opaque_LOWL_metadata;

typedef struct SMOP__p6opaque_struct {
  SMOP__Object__BASE
  SMOP__Object* instanceof;
  SMOP__Object* WHENCE;
  SMOP__p6opaque_LOWL_metadata* metadata;
  SMOP__p6opaque_LOWL_HoH instance;
} SMOP__p6opaque_struct;

typedef struct SMOP__p6opaque_proxy_struct {
  SMOP__Object__BASE
  SMOP__Object* object;
} SMOP__p6opaque_proxy_struct;

typedef struct SMOP__p6opaque_canlazylist_struct {
  SMOP__Object__BASE
  SMOP__Object* object;
  SMOP__Object* name;
  SMOP__Object* capture;
} SMOP__p6opaque_canlazylist_struct;

typedef struct SMOP__p6opaque_canlazyiterator_struct {
  SMOP__Object__BASE
  SMOP__Object* object;
  SMOP__Object* name;
  SMOP__Object* capture;
  int count_methods_optimal;
  int count_methods;
} SMOP__p6opaque_canlazyiterator_struct;

SMOP__Object* SMOP__p6opaque__RI;
SMOP__Object* SMOP__p6opaque__Scalar__whence;
SMOP__Object* SMOP__p6opaque__Array__isa;
SMOP__Object* SMOP__p6opaque__Array__does;
SMOP__Object* SMOP__p6opaque__Hash__private_storage;
SMOP__Object* SMOP__p6opaque__Hash__class_storage;
SMOP__Object* SMOP__p6opaque__Array__methods;
SMOP__Object* SMOP__p6opaque__Array__attributes;
SMOP__Object* SMOP__p6opaque__Scalar__package;
SMOP__Object* SMOP__p6opaque__Hash__properties;
SMOP__Object* SMOP__p6opaque__List__can;
SMOP__Object* SMOP__p6opaque__Iterator__can;

static SMOP__Object* p6opaque_message(SMOP__Object* interpreter,
                                      SMOP__ResponderInterface* self,
                                      SMOP__Object* identifier,
                                      SMOP__Object* capture) {
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  if (identifier == SMOP__ID__REPR_CREATE) {
    ret = smop_lowlevel_alloc(sizeof(SMOP__p6opaque_struct));
    ret->RI = (SMOP__ResponderInterface*)SMOP__p6opaque__RI;
  } else if (identifier == SMOP__ID__REPR_how) {
    fprintf(stderr,"[SMOP p6opaque] .^!how not implemented");
    ret = SMOP__NATIVE__bool_true;
  } else {
    // as we want to support different captures, we'll start to use a
    // sm0p frame in here, to use the capture as high-level and not as
    // low-level
    ret = SMOP__NATIVE__bool_true;
    SMOP__Object* frame;
    SMOP__Object* continuation = SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                                               SMOP__ID__continuation, interpreter);
    $frame = q:sm0p {
      $capture;
      $identifier;
      $capture.SMOP__ID__invocant();
      SMOP__SLIME__CurrentFrame.copy(1);
      SMOP__SLIME__CurrentFrame.move_capturize(SMOP__SLIME__Capturize.new(1,(),(),1));
      SMOP__p6opaque__RI.SMOP__ID__REPR_how();
      SMOP__SLIME__CurrentFrame.copy(1);
      SMOP__SLIME__CurrentFrame.move_responder(2,2);
      SMOP__SLIME__CurrentFrame.move_capturize(SMOP__SLIME__Capturize.new(2,(6,7,8),(),1));
      SMOP__NATIVE__bool_true.SMOP__ID__dispatch();
      $continuation;
      SMOP__SLIME__CurrentFrame.move_capturize(SMOP__SLIME__Capturize.new(1,(2),(),1));
      $continuation.setr();
      $interpreter.goto(|$continuation);
    };
    SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                  SMOP__ID__goto,
                  frame);

  }
  SMOP_RELEASE(interpreter,capture);
  return ret;
}

static SMOP__Object* p6opaque_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if ((SMOP__Object*)SMOP__p6opaque__RI != obj) {
    smop_lowlevel_refcnt_inc(interpreter, responder, obj);
  }
  return obj;
}

static SMOP__Object* p6opaque_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if ((SMOP__Object*)SMOP__p6opaque__RI != obj) {
    smop_lowlevel_refcnt_dec(interpreter, responder, obj);
  }
  return obj;
}



void smop_p6opaque_init() {

  SMOP__p6opaque__RI = calloc(1,sizeof(SMOP__ResponderInterface));
  assert(SMOP__p6opaque__RI);
  ((SMOP__ResponderInterface*)SMOP__p6opaque__RI)->MESSAGE = p6opaque_message;
  ((SMOP__ResponderInterface*)SMOP__p6opaque__RI)->REFERENCE = p6opaque_reference;
  ((SMOP__ResponderInterface*)SMOP__p6opaque__RI)->RELEASE = p6opaque_release;

}

void smop_p6opaque_destr() {

  free(SMOP__p6opaque__RI);

}
