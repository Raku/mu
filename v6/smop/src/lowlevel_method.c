#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_oo.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

SMOP__Object* SMOP__OO__LOWL__Method;

typedef struct SMOP__OO__LOWL__Method_struct {
  SMOP__Object__BASE
  int multi;
  SMOP__Object* name;
  SMOP__Object* signature;
  SMOP__Object* (*code) (SMOP__Object* interpreter,
                         SMOP__Object* method,
                         SMOP__Object* capture);
} SMOP__OO__LOWL__Method_struct;

static SMOP__Object* lowlevel_method_message(SMOP__Object* interpreter,
                                             SMOP__ResponderInterface* responder,
                                             SMOP__Object* identifier,
                                             SMOP__Object* capture) {
  if (SMOP__ID__call == identifier) {
    SMOP__Object* method = SMOP__NATIVE__capture_invocant(interpreter,capture);
    SMOP__OO__LOWL__Method_struct* m = (SMOP__OO__LOWL__Method_struct*)method;

    smop_lowlevel_rdlock(method);
    SMOP__Object* (*code) (SMOP__Object* interpreter,
                           SMOP__Object* method,
                           SMOP__Object* capture) = m->code;
    smop_lowlevel_unlock(method);

    code(interpreter,method,capture);

    SMOP_RELEASE(interpreter,method);
  } else if (SMOP__ID__DESTROYALL == identifier) {
    SMOP__OO__LOWL__Method_struct* m = (SMOP__OO__LOWL__Method_struct*)capture;
    
    smop_lowlevel_wrlock(capture);
    SMOP__Object* name = m->name; m->name = NULL;
    SMOP__Object* signature = m->signature; m->signature = NULL;
    smop_lowlevel_unlock(capture);

    SMOP_RELEASE(interpreter,name);
    SMOP_RELEASE(interpreter,signature);
  } else {
    fprintf(stderr,"Unknown identifier in lowlevel method object invocation.\n");
    SMOP_RELEASE(interpreter,capture);
  }
  return SMOP__NATIVE__bool_false;
}

static SMOP__Object* lowlevel_method_reference(SMOP__Object* interpreter,
                                               SMOP__ResponderInterface* responder,
                                               SMOP__Object* value) {
  if (SMOP__OO__LOWL__Method != value) {
    return smop_lowlevel_refcnt_inc(interpreter,responder,value);
  } else {
    return value;
  }
}

static SMOP__Object* lowlevel_method_release(SMOP__Object* interpreter,
                                             SMOP__ResponderInterface* responder,
                                             SMOP__Object* value) {
  if (SMOP__OO__LOWL__Method != value) {
    return smop_lowlevel_refcnt_dec(interpreter,responder,value);
  } else {
    return value;
  }
}

SMOP__Object* SMOP__OO__LOWL__Method_create(int multi,
                                            SMOP__Object* name,
                                            SMOP__Object* signature,
                                            SMOP__Object* (*code) (SMOP__Object* interpreter,
                                                                   SMOP__Object* method,
                                                                   SMOP__Object* capture)) {
  SMOP__Object* ret = smop_lowlevel_alloc(sizeof(SMOP__OO__LOWL__Method_struct));
  ret->RI = (SMOP__ResponderInterface*)SMOP__OO__LOWL__Method;

  SMOP__OO__LOWL__Method_struct* m = (SMOP__OO__LOWL__Method_struct*)ret;
  m->name = name;
  m->signature = signature;
  m->multi = multi;
  m->code = code;

  return ret;

}


void smop_lowlevel_method_init() {
  SMOP__OO__LOWL__Method = malloc(sizeof(SMOP__ResponderInterface));
  assert(SMOP__OO__LOWL__Method);
  ((SMOP__ResponderInterface*)SMOP__OO__LOWL__Method)->RI = NULL;
  ((SMOP__ResponderInterface*)SMOP__OO__LOWL__Method)->MESSAGE = lowlevel_method_message;
  ((SMOP__ResponderInterface*)SMOP__OO__LOWL__Method)->REFERENCE = lowlevel_method_reference;
  ((SMOP__ResponderInterface*)SMOP__OO__LOWL__Method)->RELEASE = lowlevel_method_release;
  ((SMOP__ResponderInterface*)SMOP__OO__LOWL__Method)->id = "SMOP Lowlevel Method";
  
}
void smop_lowlevel_method_destr() {
  free(SMOP__OO__LOWL__Method);
}
