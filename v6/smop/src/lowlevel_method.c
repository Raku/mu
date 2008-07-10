#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

SMOP__Object* SMOP__S1P__Method;

/*
 * SMOP__S1P__Method passes all this items as the ownership of the
 * objects. Which means that the method is responsible for releasing
 * the object.
 *
 * This is important to allow the method to pass the object to another
 * call without having to do an additional call to REFERENCE.
 */
typedef struct SMOP__S1P__Method_struct {
  SMOP__Object__BASE
  int multi;
  SMOP__Object* name;
  SMOP__Object* signature;
  SMOP__Object* (*code) (SMOP__Object* interpreter,
                         SMOP__Object* method,
                         SMOP__Object* responder,
                         SMOP__Object* identifier,
                         SMOP__Object* capture);
} SMOP__S1P__Method_struct;

static SMOP__Object* lowlevel_method_message(SMOP__Object* interpreter,
                                             SMOP__ResponderInterface* responder,
                                             SMOP__Object* identifier,
                                             SMOP__Object* capture) {
  if (SMOP__ID__call == identifier) {
    SMOP__Object* method = SMOP__NATIVE__capture_invocant(interpreter,capture);
    SMOP__S1P__Method_struct* m = (SMOP__S1P__Method_struct*)method;

    SMOP__Object* responder = SMOP__NATIVE__capture_positional(interpreter,capture,0);
    SMOP__Object* identifier = SMOP__NATIVE__capture_positional(interpreter,capture,1);
    SMOP__Object* actualcap = SMOP__NATIVE__capture_positional(interpreter,capture,2);

    smop_lowlevel_rdlock(method);
    SMOP__Object* (*code) (SMOP__Object* interpreter,
                           SMOP__Object* method,
                           SMOP__Object* responder,
                           SMOP__Object* identifier,
                           SMOP__Object* capture) = m->code;
    smop_lowlevel_unlock(method);

    code(interpreter,method,responder,identifier,actualcap);

    SMOP_RELEASE(interpreter,capture);
    SMOP_RELEASE(interpreter,method);

  } else if (SMOP__ID__DESTROYALL == identifier) {
    SMOP__Object* method = SMOP__NATIVE__capture_invocant(interpreter,capture);
    SMOP__S1P__Method_struct* m = (SMOP__S1P__Method_struct*)method;
    
    smop_lowlevel_wrlock(capture);
    SMOP__Object* name = m->name; m->name = NULL;
    SMOP__Object* signature = m->signature; m->signature = NULL;
    smop_lowlevel_unlock(capture);

    SMOP_RELEASE(interpreter,method);

    SMOP_RELEASE(interpreter,name);
    SMOP_RELEASE(interpreter,signature);
    SMOP_RELEASE(interpreter,capture);
  } else {
    fprintf(stderr,"Unknown identifier in lowlevel method object invocation.\n");
    SMOP_RELEASE(interpreter,capture);
  }
  return SMOP__NATIVE__bool_false;
}

static SMOP__Object* lowlevel_method_reference(SMOP__Object* interpreter,
                                               SMOP__ResponderInterface* responder,
                                               SMOP__Object* value) {
  if (SMOP__S1P__Method != value) {
    return smop_lowlevel_refcnt_inc(interpreter,responder,value);
  } else {
    return value;
  }
}

static SMOP__Object* lowlevel_method_release(SMOP__Object* interpreter,
                                             SMOP__ResponderInterface* responder,
                                             SMOP__Object* value) {
  if (SMOP__S1P__Method != value) {
    return smop_lowlevel_refcnt_dec(interpreter,responder,value);
  } else {
    return value;
  }
}

SMOP__Object* SMOP__S1P__Method_create(int multi,
                                            SMOP__Object* name,
                                            SMOP__Object* signature,
                                            SMOP__Object* (*code) (SMOP__Object* interpreter,
                                                                   SMOP__Object* method,
                                                                   SMOP__Object* responder,
                                                                   SMOP__Object* identifier,
                                                                   SMOP__Object* capture)) {
  SMOP__Object* ret = smop_lowlevel_alloc(sizeof(SMOP__S1P__Method_struct));
  ret->RI = (SMOP__ResponderInterface*)SMOP__S1P__Method;

  SMOP__S1P__Method_struct* m = (SMOP__S1P__Method_struct*)ret;
  m->name = name;
  m->signature = signature;
  m->multi = multi;
  m->code = code;

  return ret;

}


void smop_lowlevel_method_init() {
  SMOP__S1P__Method = malloc(sizeof(SMOP__ResponderInterface));
  assert(SMOP__S1P__Method);
  ((SMOP__ResponderInterface*)SMOP__S1P__Method)->RI = NULL;
  ((SMOP__ResponderInterface*)SMOP__S1P__Method)->MESSAGE = lowlevel_method_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Method)->REFERENCE = lowlevel_method_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Method)->RELEASE = lowlevel_method_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Method)->id = "SMOP Lowlevel Method";
  
}
void smop_lowlevel_method_destr() {
  free(SMOP__S1P__Method);
}
