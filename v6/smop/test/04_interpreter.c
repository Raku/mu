#include <stdlib.h>
#include <stdio.h>
#include <smop.h>
#include <smop_lowlevel.h>

static SMOP__Object* custom_MESSAGE(SMOP__Object* stack,
                                    SMOP__ResponderInterface* self,
                                    SMOP__Object* identifier,
                                    SMOP__Object* capture) {
  if (identifier == SMOP__ID__has_next) {
    printf("ok 4 - has_next called.\n");
  } else if (identifier == SMOP__ID__next) {
    printf("ok 5 - next called.\n");
  } else if (identifier == SMOP__ID__eval) {
    printf("ok 6 - eval called.\n");
  } else if (identifier == SMOP__ID__DESTROYALL) {
    printf("ok 8 - DESTROYALL called.\n");
  } else {
    printf("not ok - unknown method called %p.\n",identifier);
  }
  return NULL;
}


int main() {
  printf("1..9\n");

  smop_init();

  SMOP__Object* intrp = SMOP_DISPATCH(NULL, SMOP_RI(SMOP__INTPTR__InterpreterInstance),
                                      SMOP__ID__new,
                                      SMOP__NATIVE__capture_create(NULL,SMOP__INTPTR__InterpreterInstance,NULL,NULL));
  
  if (!intrp) {
    printf("not ");
  }
  printf("ok 1 - interpreter created.\n");

  SMOP__Object* obj = smop_lowlevel_alloc(sizeof(SMOP__ResponderInterface));
  SMOP__ResponderInterface* ri = (SMOP__ResponderInterface*)obj;
  ri->MESSAGE = custom_MESSAGE;
  ri->REFERENCE = smop_lowlevel_refcnt_inc;
  ri->RELEASE = smop_lowlevel_refcnt_dec;

  printf("ok 2 - continuation created.\n");

  printf("ok 3 - goto.\n");

  SMOP_DISPATCH(NULL, ri,
                SMOP__ID__has_next,
                SMOP__NATIVE__capture_create(intrp,obj,NULL,NULL));

  SMOP_DISPATCH(NULL, ri,
                SMOP__ID__next,
                SMOP__NATIVE__capture_create(intrp,obj,NULL,NULL));
  
  SMOP_DISPATCH(NULL, ri,
                SMOP__ID__eval,
                SMOP__NATIVE__capture_create(intrp,obj,NULL,NULL));

  printf("ok 7 - delegated.\n");

  SMOP_RELEASE(intrp,intrp);

  printf("ok 9 - should be destroyed.\n");

  smop_destr();
  return 0;
}
