#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_oo.h>
#include <stdlib.h>

SMOP__Object* SMOP__OO__LOWL__Method;


SMOP__Object* SMOP__OO__LOWL__Method_create(int multi,
                                            SMOP__Object* name,
                                            SMOP__Object* signature,
                                            SMOP__Object* (*code) (SMOP__Object* interpreter,
                                                                   SMOP__Object* method,
                                                                   SMOP__Object* capture)) {
  
}


void smop_lowlevel_method_init() {
}
void smop_lowlevel_method_destr() {
}
