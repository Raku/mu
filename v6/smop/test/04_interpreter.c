#include <stdlib.h>
#include <stdio.h>
#include <smop.h>
#include <smop_lowlevel.h>

static SMOP__Object* custom_MESSAGE(SMOP__Object* stack,
                                    SMOP__ResponderInterface* self,
                                    SMOP__Object* identifier,
                                    SMOP__Object* capture) {
  if (identifier == SMOP__ID__eval) {
    printf("ok 4 - eval called.\n");
  } else if (identifier == SMOP__ID__DESTROYALL) {
    printf("ok 6 - DESTROYALL called.\n");
  } else {
    printf("not ok - unknown method called %p.\n",identifier);
  }
  SMOP_RELEASE(stack,capture);
  return SMOP__NATIVE__bool_false;
}


int main() {
  printf("1..7\n");

  smop_init();

  SMOP__Object* intrp = SMOP_DISPATCH(SMOP__INTPTR__InterpreterInstance, SMOP_RI(SMOP__INTPTR__InterpreterInstance),
                                      SMOP__ID__new,
                                      SMOP__NATIVE__capture_create(SMOP__INTPTR__InterpreterInstance,
                                                                   SMOP__INTPTR__InterpreterInstance,NULL,NULL));
  
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

  SMOP_DISPATCH(intrp, ri,
                SMOP__ID__eval,
                SMOP__NATIVE__capture_create(intrp,SMOP_REFERENCE(intrp,obj),NULL,NULL));

  SMOP_RELEASE(intrp,obj);

  printf("ok 5 - delegated.\n");

  SMOP_DISPATCH(intrp, SMOP_RI(intrp),
                SMOP__ID__loop, SMOP__NATIVE__capture_create(intrp,
                                                             SMOP_REFERENCE(intrp,intrp),
                                                             NULL,
                                                             NULL));


  SMOP_RELEASE(SMOP__INTPTR__InterpreterInstance,intrp);

  printf("ok 7 - should be destroyed.\n");

  smop_destr();
  return 0;
}
