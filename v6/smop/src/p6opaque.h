#ifndef p6opaque_H
#define p6opaque_H

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

extern SMOP__Object* SMOP__p6opaque__Scalar__whence;
extern SMOP__Object* SMOP__p6opaque__Array__isa;
extern SMOP__Object* SMOP__p6opaque__Array__does;
extern SMOP__Object* SMOP__p6opaque__Hash__private_storage;
extern SMOP__Object* SMOP__p6opaque__Hash__class_storage;
extern SMOP__Object* SMOP__p6opaque__Array__methods;
extern SMOP__Object* SMOP__p6opaque__Array__attributes;
extern SMOP__Object* SMOP__p6opaque__Scalar__package;
extern SMOP__Object* SMOP__p6opaque__Hash__properties;
extern SMOP__Object* SMOP__p6opaque__List__can;
extern SMOP__Object* SMOP__p6opaque__Iterator__can;


// methods introspection.
extern void p6opaque_methods_init();
extern void p6opaque_methods_destr();
extern SMOP__Object* p6opaque_methods_array_create(SMOP__Object* interpreter,
                                                   SMOP__Object* invocant);

#endif
