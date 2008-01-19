
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
}


int main() {
  printf("1..9\n");

  smop_init();

  SMOP__Object* intrp = SMOP_DISPATCH(NULL, SMOP_RI(SMOP__INTPTR__InterpreterInstance),
                                      SMOP__ID__new,
                                      smop__intptr__invocant_capture_new(SMOP__INTPTR__InterpreterInstance));
  
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

  SMOP__Object* goto_capture = smop__intptr__goto_capture_new(intrp, node);
  SMOP_DISPATCH(NULL, SMOP_RI(intrp),
                SMOP__ID__goto, goto_capture);
  SMOP_RELEASE(NULL,goto_capture);

  printf("ok 3 - goto.\n");

  SMOP_DISPATCH(NULL, SMOP_RI(intrp),
                SMOP__ID__has_next,
                smop__intptr__invocant_capture_new(intrp));

  SMOP_DISPATCH(NULL, SMOP_RI(intrp),
                SMOP__ID__next,
                smop__intptr__invocant_capture_new(intrp));
  
  SMOP_DISPATCH(NULL, SMOP_RI(intrp),
                SMOP__ID__eval,
                smop__intptr__invocant_capture_new(intrp));

  printf("ok 7 - delegated.\n");

  SMOP_RELEASE(intr);

  printf("ok 9 - should be destroyed.\n");

  smop_destr();
}
