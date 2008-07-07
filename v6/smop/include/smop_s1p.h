#ifndef SMOP_S1P_H
#define SMOP_S1P_H


extern SMOP__Object* SMOP__S1P__Scalar;
extern SMOP__Object* SMOP__S1P__Array;
extern SMOP__Object* SMOP__S1P__IO;
extern SMOP__Object* SMOP__S1P__Hash;
extern SMOP__Object* SMOP__S1P__Code;
extern SMOP__Object* SMOP__S1P__Package;
extern SMOP__Object* SMOP__S1P__Method;
extern SMOP__Object* SMOP__S1P__SubMethod;
extern SMOP__Object* SMOP__S1P__Signature;


SMOP__Object* SMOP__S1P__Method_create(int multi,
                                       SMOP__Object* name,
                                       SMOP__Object* signature,
                                       SMOP__Object* (*code) (SMOP__Object* interpreter,
                                                              SMOP__Object* method,
                                                              SMOP__Object* capture));

SMOP__Object* SMOP__S1P__Scalar_create(SMOP__Object* initial_value);
SMOP__Object* SMOP__S1P__Scalar_FETCH(SMOP__Object* object);
SMOP__Object* SMOP__S1P__Scalar_STORE(SMOP__Object* object, SMOP__Object* new_value);
SMOP__Object* SMOP__S1P__IO_create(void);



#endif
