#include <stdio.h>
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

static void runloop(SMOP__Object* invocant,
                    SMOP__Object* cont) {

    SMOP__Object* has_next;
    if (cont) {
      has_next = SMOP_DISPATCH(invocant, SMOP_RI(cont), SMOP__ID__has_next,
                               SMOP__NATIVE__capture_create(invocant, SMOP_REFERENCE(invocant,cont), NULL, NULL));
    } else {
      has_next = SMOP__NATIVE__bool_false;
    }

    while (cont && has_next == SMOP__NATIVE__bool_true) {
      SMOP_DISPATCH(invocant, SMOP_RI(cont), SMOP__ID__next,
                    SMOP__NATIVE__capture_create(invocant, SMOP_REFERENCE(invocant,cont), NULL, NULL));
      SMOP_DISPATCH(invocant, SMOP_RI(cont), SMOP__ID__eval,
                    SMOP__NATIVE__capture_create(invocant, SMOP_REFERENCE(invocant,cont), NULL, NULL));
      
      
      if (invocant != SMOP__INTPTR__InterpreterInstance) {
        smop_lowlevel_rdlock(invocant);
        cont = ((interpreter_instance_struct*)invocant)->continuation;
        smop_lowlevel_unlock(invocant);
      }

      if (cont) {
        has_next = SMOP_DISPATCH(invocant, SMOP_RI(cont), SMOP__ID__has_next,
                                 SMOP__NATIVE__capture_create(invocant, SMOP_REFERENCE(invocant,cont), NULL, NULL));
      } else {
        has_next = SMOP__NATIVE__bool_false;
      }

    }

    SMOP_RELEASE(invocant, has_next);

}

static SMOP__Object* prototype_interpreter_message(SMOP__Object* interpreter,
                                                   SMOP__Object* identifier,
                                                   SMOP__Object* capture) {
  if (identifier == SMOP__ID__new) {
    SMOP__Object* ret = smop_lowlevel_alloc(sizeof(interpreter_instance_struct));
    ret->RI = (SMOP__ResponderInterface*)SMOP__INTPTR__InterpreterInstance;
    SMOP_RELEASE(interpreter,capture);
    return ret;

  } else if (identifier == SMOP__ID__continuation) {
    /* continuation SMOP__ID__continuation: ;
     * returns false
     */

  } else if (identifier == SMOP__ID__goto) {
    /* goto SMOP__INTPTR__InterpreterInstance: $target;
     * 
     * Calling goto on the prototype recurses in the C stack.
     */
    SMOP__Object* target;
    if (SMOP_RI(capture) == (SMOP__ResponderInterface*)SMOP__NATIVE__capture) {
      target = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
    } else {
      target = SMOP_REFERENCE(interpreter,capture);
    }
    if (target != SMOP__NATIVE__bool_false) 
      runloop(SMOP__INTPTR__InterpreterInstance, target);

    SMOP_RELEASE(interpreter,target);

  } else if (identifier == SMOP__ID__has_next) {

  } else if (identifier == SMOP__ID__DESTROYALL) {
    interpreter_instance_struct* inst = (interpreter_instance_struct*)capture;
    smop_lowlevel_wrlock(capture);
    SMOP__Object* cont = inst->continuation; inst->continuation = NULL;
    smop_lowlevel_unlock(capture);
    if (cont) SMOP_RELEASE(interpreter,cont);

  } else {
    if (SMOP_RI(identifier) == SMOP_RI(SMOP__ID__new)) {
      fprintf(stderr,"[SMOP__INTPTR__InterpreterInstance] called %s on prototype.\n",(char*)identifier->data);
    } else {
      fprintf(stderr,"[SMOP__INTPTR__InterpreterInstance] called concrete message on prototype.\n");
    }
  }
  SMOP_RELEASE(interpreter,capture);
  return SMOP__NATIVE__bool_false;
}


static SMOP__Object* interpreter_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {

  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  SMOP__Object* invocant;
  if (SMOP_RI(capture) == (SMOP__ResponderInterface*)SMOP__NATIVE__capture) {
    invocant = SMOP__NATIVE__capture_invocant(interpreter, capture);
  } else {
    SMOP_REFERENCE(interpreter, interpreter);
    invocant = interpreter;
  }

  if (invocant == SMOP__INTPTR__InterpreterInstance)
    return prototype_interpreter_message(interpreter, identifier, capture);

  if (identifier == SMOP__ID__new) {
    ret = smop_lowlevel_alloc(sizeof(interpreter_instance_struct));
    ret->RI = (SMOP__ResponderInterface*)SMOP__INTPTR__InterpreterInstance;

  } else if (identifier == SMOP__ID__goto) {
    /* goto $interpreter: $target;
     * 
     * This set the continuation of the invocant interpreter to the
     * given target. If there is a current target, it will be
     * released.
     */
    SMOP__Object* target;
    if (SMOP_RI(capture) == (SMOP__ResponderInterface*)SMOP__NATIVE__capture) {
      target = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
    } else {
      target = SMOP_REFERENCE(interpreter,capture);
    }
    smop_lowlevel_wrlock(invocant);
    SMOP__Object* cont = ((interpreter_instance_struct*)invocant)->continuation;
    ((interpreter_instance_struct*)invocant)->continuation = target;
    smop_lowlevel_unlock(invocant);
    
    if (cont) SMOP_RELEASE(interpreter,cont);

  } else if (identifier == SMOP__ID__setr) {
    /* setr $interpreter: $result;
     *
     * Delegates the call to the current continuation (if there is one).
     */
    SMOP__Object* result = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
    smop_lowlevel_rdlock(invocant);
    SMOP__Object* cont = ((interpreter_instance_struct*)invocant)->continuation;
    smop_lowlevel_unlock(invocant);
    if (cont)
      ret = SMOP_DISPATCH(interpreter, SMOP_RI(cont), SMOP__ID__setr,
                          SMOP__NATIVE__capture_create(interpreter, SMOP_REFERENCE(interpreter,cont),
                                                       (SMOP__Object*[]){result, NULL},NULL));

  } else if (identifier == SMOP__ID__has_next) {
    /* has_next $interpreter: ;
     *
     * This call *must* return a native bool. It will be checked
     * against native true value, else it will be understood as false.
     *
     * Delegates the call to the current continuation (if there is one).
     */
    smop_lowlevel_rdlock(invocant);
    SMOP__Object* cont = ((interpreter_instance_struct*)invocant)->continuation;
    smop_lowlevel_unlock(invocant);
    if (cont) {
      ret = SMOP_DISPATCH(interpreter, SMOP_RI(cont), SMOP__ID__has_next,
                          SMOP__NATIVE__capture_create(interpreter, SMOP_REFERENCE(interpreter,cont), NULL, NULL));
    } else {
      ret = SMOP__NATIVE__bool_false;
    }

  } else if (identifier == SMOP__ID__next) {
    /* next $interpreter: ;
     *
     * Delegates the call to the current continuation (if there is one).
     */
    smop_lowlevel_rdlock(invocant);
    SMOP__Object* cont = ((interpreter_instance_struct*)invocant)->continuation;
    smop_lowlevel_unlock(invocant);
    if (cont)
      ret = SMOP_DISPATCH(interpreter, SMOP_RI(cont), SMOP__ID__next,
                          SMOP__NATIVE__capture_create(interpreter, SMOP_REFERENCE(interpreter,cont), NULL, NULL));

  } else if (identifier == SMOP__ID__eval) {
    /* eval $interpreter: ;
     *
     * Delegates the call to the current continuation (if there is one).
     */
    smop_lowlevel_rdlock(invocant);
    SMOP__Object* cont = ((interpreter_instance_struct*)invocant)->continuation;
    smop_lowlevel_unlock(invocant);
    if (cont)
      ret = SMOP_DISPATCH(interpreter, SMOP_RI(cont), SMOP__ID__eval,
                          SMOP__NATIVE__capture_create(interpreter, SMOP_REFERENCE(interpreter,cont), NULL, NULL));

  } else if (identifier == SMOP__ID__continuation) {
    /* continuation $interpreter: ;
     *
     * returns the current continuation (if there is one).
     */
    smop_lowlevel_rdlock(invocant);
    SMOP__Object* cont = ((interpreter_instance_struct*)invocant)->continuation;
    smop_lowlevel_unlock(invocant);
    if (cont)
      ret = SMOP_REFERENCE(interpreter,cont);

  } else if (identifier == SMOP__ID__debug) {
    /* debug $interpreter: ;
     *
     * Delegates the call to the current continuation (if there is one).
     */
    smop_lowlevel_rdlock(invocant);
    SMOP__Object* cont = ((interpreter_instance_struct*)invocant)->continuation;
    smop_lowlevel_unlock(invocant);
    if (cont)
      ret = SMOP_DISPATCH(interpreter, SMOP_RI(cont), SMOP__ID__debug,
                          SMOP__NATIVE__capture_create(interpreter, SMOP_REFERENCE(interpreter,cont), NULL, NULL));

  } else if (identifier == SMOP__ID__jail) {
    /* jail $interpreter: ;
     *
     * Delegates the call to the current continuation (if there is one).
     */
    smop_lowlevel_rdlock(invocant);
    SMOP__Object* cont = ((interpreter_instance_struct*)invocant)->continuation;
    smop_lowlevel_unlock(invocant);
    if (cont)
      ret = SMOP_DISPATCH(interpreter, SMOP_RI(cont), SMOP__ID__jail,
                          SMOP__NATIVE__capture_create(interpreter, SMOP_REFERENCE(interpreter,cont), NULL, NULL));

  } else if (identifier == SMOP__ID__loop) {
    /* loop $interpreter: ;
     *
     * Goes through the continuation while has_next.
     */

    smop_lowlevel_rdlock(invocant);
    SMOP__Object* cont = ((interpreter_instance_struct*)invocant)->continuation;
    smop_lowlevel_unlock(invocant);

    if (cont != SMOP__NATIVE__bool_false) {
      runloop(invocant, cont);
    }

  } else if (identifier == SMOP__ID__DESTROYALL) {
    interpreter_instance_struct* inst = (interpreter_instance_struct*)capture;
    smop_lowlevel_wrlock(capture);
    SMOP__Object* cont = inst->continuation; inst->continuation = NULL;
    smop_lowlevel_unlock(capture);
    if (cont) SMOP_RELEASE(interpreter,cont);
  }
  SMOP_RELEASE(interpreter, invocant);
  SMOP_RELEASE(interpreter, capture);
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
  ((SMOP__ResponderInterface*)SMOP__INTPTR__InterpreterInstance)->id = "SMOP Intepreter Instance";

}

void smop_interpreter_destr() {

  // destroy the interpreter prototype
  free(SMOP__INTPTR__InterpreterInstance);
}
