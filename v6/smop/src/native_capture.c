#include <smop.h>
#include <smop_lowlevel.h>

/* The native capture prototype is at the same time a responder
 * interface. And the prototype is not subject to garbage
 * collection. Each capture instance, however uses the smop_lowlevel,
 * and, therefore, is subject to gc.
 */
SMOP__Object* SMOP__NATIVE__capture;

typedef struct native_capture_struct {
  SMOP__Object__BASE
  SMOP__Object* invocant;
  SMOP__Object** positional;
  SMOP__Object** named_keys;
  SMOP__Object** named_values;
} native_capture_struct;

/* A constant empty capture will be created. Understanding that a
 * capture is readonly, everytime someone tryies to create one using
 * "new", the constant empty capture will be returned.
 */
static SMOP__Object* smop_native_empty_capture;

static SMOP__Object* capture_message(SMOP__Object* stack,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {
  SMOP__Object* ret = NULL;
  swtich (identifier) {
  SMOP__ID__new:
    ret = smop_native_empty_capture;
    break;
  }
  return ret;
}

static SMOP__Object* capture_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if (responder != obj && smop_native_empty_capture != obj) {
    smop_lowlevel_refcnt_inc(interpreter, responder, obj);
  }
  return obj;
}

static SMOP__Object* capture_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if (responder != obj && smop_native_empty_capture != obj) {
    smop_lowlevel_refcnt_dec(interpreter, responder, obj);
  }
  return obj;
}



void smop_native_capture_init() {

  // initialize the capture prototype
  SMOP__NATIVE__capture = calloc(1, sizeof(SMOP__ResponderInterface));
  assert(SMOP__NATIVE__capture);
  SMOP__NATIVE__capture->MESSAGE = capture_message;
  SMOP__NATIVE__capture->REFERENCE = capture_reference;
  SMOP__NATIVE__capture->RELEASE = capture_release;

  smop_native_empty_capture = calloc(1, sizeof(native_capture_struct));
  assert(smop_native_empty_capture);
  smop_native_empty_capture->RI = SMOP__NATIVE__capture;

}

void smop_native_capture_destr() {

  // destroy the capture prototype
  free(SMOP__NATIVE__capture);

}
