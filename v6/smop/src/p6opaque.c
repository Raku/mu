
#include <smop.h>
#include <smop_lowlevel.h>

typedef struct SMOP__p6opaque_LOWL_A {
  int count;
  SMOP__Object** array;
} SMOP__p6opaque_LOWL_A;

typedef struct SMOP__p6opaque_LOWL_P {
  SMOP__Object* key
  SMOP__Object* value;
} SMOP__p6opaque_LOWL_P;

typedef struct SMOP__p6opaque_LOWL_H {
  int count;
  SMOP__p6opaque_LOWL_P* array;
} SMOP__p6opaque_LOWL_H;

typedef struct SMOP__p6opaque_LOWL_PoA {
  SMOP__Object* key
  SMOP__p6opaque_LOWL_A value;
} SMOP__p6opaque_LOWL_PoA;

typedef struct SMOP__p6opaque_LOWL_PoH {
  SMOP__Object* key
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


void smop_p6opaque_init() {

}

void smop_p6opaque_destr() {

}
