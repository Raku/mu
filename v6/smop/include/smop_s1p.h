#ifndef SMOP_S1P_H
#define SMOP_S1P_H


extern SMOP__Object* SMOP__S1P__Scalar;
extern SMOP__Object* SMOP__S1P__Array;
extern SMOP__Object* SMOP__S1P__IO;
extern SMOP__Object* SMOP__S1P__Str;
extern SMOP__Object* SMOP__S1P__Hash;
extern SMOP__Object* SMOP__S1P__Code;
extern SMOP__Object* SMOP__S1P__CCode;
extern SMOP__Object* SMOP__S1P__Attribute;
extern SMOP__Object* SMOP__S1P__Package;
extern SMOP__Object* SMOP__S1P__Method;
extern SMOP__Object* SMOP__S1P__SubMethod;
extern SMOP__Object* SMOP__S1P__Signature;
extern SMOP__Object* SMOP__S1P__RootNamespace;
extern SMOP__Object* SMOP__GlobalInterpreter;


SMOP__Object* SMOP__S1P__Method_create(int multi,
                                       SMOP__Object* name,
                                       SMOP__Object* signature,
                                       SMOP__Object* (*code) (SMOP__Object* interpreter,
                                                              SMOP__Object* method,
                                                              SMOP__Object* responder,
                                                              SMOP__Object* identifier,
                                                              SMOP__Object* capture));


SMOP__Object* SMOP__S1P__CCode_create(SMOP__Object* (*code) (SMOP__Object* interpreter,
                                                             SMOP__Object* code,
                                                             SMOP__Object* capture));

SMOP__Object* SMOP__S1P__Scalar_create(SMOP__Object* initial_value);
SMOP__Object* SMOP__S1P__Str_createn(char* data,int len);
SMOP__Object* SMOP__S1P__Str_create(char* data);
char* SMOP__S1P__Str_c_str(SMOP__Object* obj);
SMOP__Object* SMOP__S1P__Scalar_FETCH(SMOP__Object* object);
SMOP__Object* SMOP__S1P__Scalar_STORE(SMOP__Object* object, SMOP__Object* new_value);
SMOP__Object* SMOP__S1P__IO_create(void);
SMOP__Object* SMOP__S1P__Hash_create(void);
SMOP__Object* SMOP__S1P__Array_create(void);


#include <assert.h>

#define ___NATIVE_CAPTURE_ONLY___ \
    assert(SMOP_RI(capture) == (SMOP__ResponderInterface*)SMOP__NATIVE__capture)

#define ___CONST_IDENTIFIER_ONLY___ \
    assert(SMOP_RI(identifier) == SMOP_RI(SMOP__ID__new))

#define ___INVOCANT_RI_SHOULD_MATCH___ \
    SMOP__Object* invocant = SMOP__NATIVE__capture_invocant(interpreter, capture); \
    assert(SMOP_RI(invocant) == (SMOP__ResponderInterface*)self)

#define ___UNKNOWN_METHOD___ \
    { \
    int identifier_size;\
    fprintf(stderr,"unkown method %s at %s line %d file %s\n",SMOP__NATIVE__idconst_fetch(identifier,&identifier_size),__func__,__LINE__,__FILE__); \
    abort(); \
    } 

#endif
