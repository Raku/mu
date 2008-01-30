#include <assert.h>
#include <stdlib.h>
#include <smop.h>
#include <smop_lowlevel.h>

/*
 * This is the deault interpreter instance. It's important to realise
 * that this is pluggable, but there's no much thing to be different
 * right here. As this object is already delegates much of its
 * features to the "current continuation" object.
 */


/* The default interpreter instance prototype is not subject to
 * garbage collection. It is initialized and destroyed explicitly
 * during smop_init and smop_destr. Both SMOP_REFERENCE and
 * SMOP_RELEASE are no-ops here. But they do use the default smop
 * lowlevel for each object. The only constant here is the prototype.
 *
 * It's important to realise that the prototype is itself the
 * responder interface.
 */
SMOP__Object* SMOP__INTPTR__InterpreterInstance;

typedef struct interpreter_instance_struct {
  SMOP__Object__BASE
  SMOP__Object* continuation;
} interpreter_instance_struct;

static SMOP__Object* interpreter_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  SMOP__Object* ret = NULL;

  if (identifier == SMOP__ID__new) {
    ret = smop_lowlevel_alloc(sizeof(interpreter_instance_struct));
    ret->RI = self;

  } else if (identifier == SMOP__ID__goto) {
    SMOP__Object* intr = SMOP__NATIVE__capture_invocant(interpreter, capture);
    SMOP__Object* targ = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
    smop_lowlevel_wrlock(capture);
    SMOP__Object* cont = ((interpreter_instance_struct*)intr)->continuation;
    ((interpreter_instance_struct*)intr)->continuation = targ;
    smop_lowlevel_unlock(capture);
    if (cont) SMOP_RELEASE(interpreter,cont);

  } else if (identifier == SMOP__ID__setr) {
    SMOP__Object* intr = SMOP__NATIVE__capture_invocant(interpreter, capture);
    SMOP__Object* targ = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
    smop_lowlevel_rdlock(intr);
    SMOP__Object* cont = ((interpreter_instance_struct*)intr)->continuation;
    smop_lowlevel_unlock(intr);
    ret = SMOP_DISPATCH(intr, SMOP_RI(cont), SMOP__ID__has_next,
                        SMOP__NATIVE__capture_create(interpreter,cont,(SMOP__Object*[]){targ, NULL},NULL));

  } else if (identifier == SMOP__ID__has_next) {
    SMOP__Object* intr = SMOP__NATIVE__capture_invocant(interpreter, capture);
    smop_lowlevel_rdlock(intr);
    SMOP__Object* cont = ((interpreter_instance_struct*)intr)->continuation;
    smop_lowlevel_unlock(intr);
    ret = SMOP_DISPATCH(interpreter, SMOP_RI(cont), SMOP__ID__has_next,
                        SMOP__NATIVE__capture_create(interpreter,cont,NULL,NULL));

  } else if (identifier == SMOP__ID__next) {
    SMOP__Object* intr = SMOP__NATIVE__capture_invocant(interpreter, capture);
    smop_lowlevel_rdlock(intr);
    SMOP__Object* cont = ((interpreter_instance_struct*)intr)->continuation;
    smop_lowlevel_unlock(intr);
    ret = SMOP_DISPATCH(interpreter, SMOP_RI(cont), SMOP__ID__next,
                        SMOP__NATIVE__capture_create(interpreter,cont,NULL,NULL));

  } else if (identifier == SMOP__ID__eval) {
    SMOP__Object* intr = SMOP__NATIVE__capture_invocant(interpreter, capture);
    smop_lowlevel_rdlock(intr);
    SMOP__Object* cont = ((interpreter_instance_struct*)intr)->continuation;
    smop_lowlevel_unlock(intr);
    ret = SMOP_DISPATCH(intr, SMOP_RI(cont), SMOP__ID__eval,
                        SMOP__NATIVE__capture_create(interpreter,cont,NULL,NULL));

  } else if (identifier == SMOP__ID__debug) {
    SMOP__Object* intr = SMOP__NATIVE__capture_invocant(interpreter, capture);
    smop_lowlevel_rdlock(intr);
    SMOP__Object* cont = ((interpreter_instance_struct*)intr)->continuation;
    smop_lowlevel_unlock(intr);
    ret = SMOP_DISPATCH(interpreter, SMOP_RI(cont), SMOP__ID__debug,
                        SMOP__NATIVE__capture_create(interpreter,cont,NULL,NULL));

  } else if (identifier == SMOP__ID__jail) {
    SMOP__Object* intr = SMOP__NATIVE__capture_invocant(interpreter, capture);
    smop_lowlevel_rdlock(intr);
    SMOP__Object* cont = ((interpreter_instance_struct*)intr)->continuation;
    smop_lowlevel_unlock(intr);
    ret = SMOP_DISPATCH(interpreter, SMOP_RI(cont), SMOP__ID__jail,
                        SMOP__NATIVE__capture_create(interpreter,cont,NULL,NULL));

  } else if (identifier == SMOP__ID__loop) {
    SMOP__Object* intr = SMOP__NATIVE__capture_invocant(interpreter, capture);

    smop_lowlevel_rdlock(intr);
    SMOP__Object* cont = ((interpreter_instance_struct*)intr)->continuation;
    smop_lowlevel_unlock(intr);

    SMOP__Object* false = SMOP__NATIVE__bool_create(0);
    SMOP__Object* has_next;
    if (cont) {
      has_next = SMOP_DISPATCH(intr, SMOP_RI(cont), SMOP__ID__has_next,
                               SMOP__NATIVE__capture_create(interpreter, cont, NULL, NULL));
    } else {
      has_next = false;
    }
    while (cont && has_next != false) {
      SMOP_DISPATCH(intr,SMOP_RI(cont), SMOP__ID__next,
                    SMOP__NATIVE__capture_create(interpreter, cont, NULL, NULL));
      SMOP_DISPATCH(intr,SMOP_RI(cont), SMOP__ID__eval,
                    SMOP__NATIVE__capture_create(interpreter, cont, NULL, NULL));
      
      smop_lowlevel_rdlock(intr);
      SMOP__Object* cont = ((interpreter_instance_struct*)intr)->continuation;
      smop_lowlevel_unlock(intr);

      if (cont) {
        has_next = SMOP_DISPATCH(intr, SMOP_RI(cont), SMOP__ID__has_next,
                                 SMOP__NATIVE__capture_create(interpreter, cont, NULL, NULL));
      } else {
        has_next = false;
      }

    }

  } else if (identifier == SMOP__ID__DESTROYALL) {
    interpreter_instance_struct* inst = (interpreter_instance_struct*)capture;
    smop_lowlevel_wrlock(capture);
    SMOP__Object* cont = inst->continuation; inst->continuation = NULL;
    smop_lowlevel_unlock(capture);
    SMOP_RELEASE(interpreter,cont);
  }
  return ret;
}

static SMOP__Object* interpreter_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if ((SMOP__Object*)responder != obj) {
    smop_lowlevel_refcnt_inc(interpreter, responder, obj);
  }
  return obj;
}

static SMOP__Object* interpreter_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if ((SMOP__Object*)responder != obj) {
    smop_lowlevel_refcnt_dec(interpreter, responder, obj);
  }
  return obj;
}


void smop_interpreter_init() {

  // initialize the interpreter prototype
  SMOP__INTPTR__InterpreterInstance = calloc(1, sizeof(SMOP__ResponderInterface));
  assert(SMOP__INTPTR__InterpreterInstance);
  ((SMOP__ResponderInterface*)SMOP__INTPTR__InterpreterInstance)->MESSAGE = interpreter_message;
  ((SMOP__ResponderInterface*)SMOP__INTPTR__InterpreterInstance)->REFERENCE = interpreter_reference;
  ((SMOP__ResponderInterface*)SMOP__INTPTR__InterpreterInstance)->RELEASE = interpreter_release;

}

void smop_interpreter_destr() {

  // destroy the interpreter prototype
  free(SMOP__INTPTR__InterpreterInstance);
}
