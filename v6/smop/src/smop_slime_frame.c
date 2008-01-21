
#include <smop.h>
#include <smop_slime.h>

SMOP__Object* SMOP__SLIME__Frame;

typedef struct smop_slime_frame_struct {
  SMOP__Object__BASE
  SMOP__Object** nodes;
  int node_count;
  int pc;
  SMOP__Object* lexical;
  SMOP__Object* back;
} smop_slime_frame_struct;

static SMOP__Object* frame_message(SMOP__Object* stack,
                                   SMOP__ResponderInterface* self,
                                   SMOP__Object* identifier,
                                   SMOP__Object* capture) {
  SMOP__Object* ret = NULL;
  swtich (identifier) {
    
  };
  return ret;
}

static SMOP__Object* frame_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if (responder != obj) {
    smop_lowlevel_refcnt_inc(interpreter, responder, obj);
  }
  return obj;
}

static SMOP__Object* frame_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if (responder != obj) {
    smop_lowlevel_refcnt_dec(interpreter, responder, obj);
  }
  return obj;
}

void smop_slime_frame_init() {
  SMOP__SLIME__Frame = calloc(1, sizeof(SMOP__ResponderInterface));
  assert(SMOP__SLIME__Frame);
  SMOP__SLIME__Frame->MESSAGE = frame_message;
  SMOP__SLIME__Frame->REFERENCE = frame_reference;
  SMOP__SLIME__Frame->RELEASE = frame_release;
}

void smop_slime_frame_destr() {
  free(SMOP__SLIME__Frame);
}
