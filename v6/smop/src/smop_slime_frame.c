#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <smop.h>
#include <smop_slime.h>
#include <smop_lowlevel.h>

SMOP__Object* SMOP__SLIME__Frame;

typedef struct smop_slime_frame_struct {
  SMOP__Object__BASE
  SMOP__Object** nodes;
  int node_count;
  int pc;
  SMOP__Object* lexical;
  SMOP__Object* back;
} smop_slime_frame_struct;

static SMOP__Object* frame_message(SMOP__Object* interpreter,
                                   SMOP__ResponderInterface* self,
                                   SMOP__Object* identifier,
                                   SMOP__Object* capture) {
  if (SMOP_RI(capture) == (SMOP__ResponderInterface*)SMOP__NATIVE__capture)
    assert(!SMOP__NATIVE__capture_may_recurse(interpreter, capture));
  SMOP__Object* ret = NULL;

  if (identifier == SMOP__ID__new) {
    ret = smop_lowlevel_alloc(sizeof(smop_slime_frame_struct));
    smop_slime_frame_struct* frame = (smop_slime_frame_struct*)ret;
    frame->RI = (SMOP__ResponderInterface*)SMOP__SLIME__Frame;
    frame->lexical = SMOP__NATIVE__capture_named(interpreter, capture, SMOP__ID__lexical);
    frame->back = SMOP__NATIVE__capture_named(interpreter, capture, SMOP__ID__back);
    frame->node_count = SMOP__NATIVE__capture_positional_count(interpreter, capture);
    frame->nodes = malloc(frame->node_count * sizeof(SMOP__Object*));
    assert(frame->nodes);
    int i;
    for (i = 0; i < frame->node_count; i++) {
      frame->nodes[i] = SMOP__NATIVE__capture_positional(interpreter, capture, i);
    }
  
  } else if (identifier ==  SMOP__ID__has_next) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      smop_lowlevel_rdlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      int nc = ((smop_slime_frame_struct*)frame)->node_count;
      void* back = ((smop_slime_frame_struct*)frame)->back;
      smop_lowlevel_unlock(frame);
      if ((nc > pc + 1) || back) {
        ret = SMOP__NATIVE__bool_true;
      } else {
        ret = SMOP__NATIVE__bool_false;
      }
      SMOP_RELEASE(interpreter,frame);
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier ==  SMOP__ID__next) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      smop_lowlevel_wrlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      int nc = ((smop_slime_frame_struct*)frame)->node_count;
      SMOP__Object* back = ((smop_slime_frame_struct*)frame)->back;
      if (nc > pc + 1) {
        ((smop_slime_frame_struct*)frame)->pc++;
        smop_lowlevel_unlock(frame);
      } else if (back) {
        SMOP__Object* node = ((smop_slime_frame_struct*)frame)->nodes[pc];
        smop_lowlevel_unlock(frame);
        SMOP__Object* r = SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__result,
                                        SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node),NULL,NULL));
        SMOP_DISPATCH(interpreter,SMOP_RI(back),SMOP__ID__setr,
                      SMOP__NATIVE__capture_create(interpreter, SMOP_REFERENCE(interpreter,back), (SMOP__Object*[]){r,NULL}, NULL));
        SMOP_DISPATCH(interpreter,SMOP_RI(interpreter),SMOP__ID__goto,
                      SMOP__NATIVE__capture_create(interpreter, SMOP_REFERENCE(interpreter,back), NULL, NULL));
        SMOP_RELEASE(interpreter, frame);
        ret = SMOP__NATIVE__bool_true;
      } else {
        smop_lowlevel_unlock(frame);
        ret = SMOP__NATIVE__bool_false;
      }
      SMOP_RELEASE(interpreter,frame);
    } else {
      ret = SMOP__NATIVE__bool_false;
    }    
  
  } else if (identifier ==  SMOP__ID__free) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      smop_lowlevel_rdlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      SMOP__Object* node = ((smop_slime_frame_struct*)frame)->nodes[pc];
      smop_lowlevel_unlock(frame);
      ret = SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__free,
                          SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node),NULL,NULL));
      SMOP_RELEASE(interpreter,frame);
    }
    ret = SMOP__NATIVE__bool_false;

  } else if (identifier ==  SMOP__ID__eval) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      smop_lowlevel_rdlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      SMOP__Object* node = ((smop_slime_frame_struct*)frame)->nodes[pc];
      smop_lowlevel_unlock(frame);
      ret = SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__eval,
                          SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node),NULL,NULL));
      SMOP__Object* res = SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__result,
                                        SMOP__NATIVE__capture_create(interpreter,
                                                                     SMOP_REFERENCE(interpreter,node),
                                                                     (SMOP__Object*[]){ret, NULL},NULL));
      if (res) SMOP_RELEASE(interpreter,res);
      SMOP_RELEASE(interpreter,frame);
      ret = SMOP__NATIVE__bool_true;
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier == SMOP__ID__jail) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      smop_lowlevel_rdlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      SMOP__Object* node = ((smop_slime_frame_struct*)frame)->nodes[pc];
      smop_lowlevel_unlock(frame);
      ret = SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__jail,
                          SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node),NULL,NULL));
      SMOP_RELEASE(interpreter,frame);
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier ==  SMOP__ID__debug) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      smop_lowlevel_rdlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      SMOP__Object* node = ((smop_slime_frame_struct*)frame)->nodes[pc];
      smop_lowlevel_unlock(frame);
      ret = SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__debug,
                          SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node),NULL,NULL));
      SMOP_RELEASE(interpreter,frame);
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier ==  SMOP__ID__result) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      SMOP__Object* count = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
      assert(SMOP_RI(count) == (SMOP__ResponderInterface*)SMOP__NATIVE__int);
      int c = SMOP__NATIVE__int_fetch(count);
      smop_lowlevel_rdlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      SMOP__Object* node = ((smop_slime_frame_struct*)frame)->nodes[pc - c];
      smop_lowlevel_unlock(frame);
      ret = SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__result,
                          SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node),NULL,NULL));
      SMOP_RELEASE(interpreter,count);
      SMOP_RELEASE(interpreter,frame);
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  }

  else if (identifier ==  SMOP__ID__setr) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      SMOP__Object* value = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
      smop_lowlevel_rdlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      SMOP__Object* node = ((smop_slime_frame_struct*)frame)->nodes[pc];
      smop_lowlevel_unlock(frame);
      SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__result,
                    SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node),(SMOP__Object*[]){value, NULL},NULL));
      SMOP_RELEASE(interpreter,frame);
      ret = SMOP__NATIVE__bool_true;
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier ==SMOP__ID__drop) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      smop_lowlevel_rdlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      SMOP__Object* back = ((smop_slime_frame_struct*)frame)->back;
      if (back) {
        SMOP__Object* node = ((smop_slime_frame_struct*)frame)->nodes[pc];
        smop_lowlevel_unlock(frame);
        SMOP__Object* r = SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__result,
                                        SMOP__NATIVE__capture_create(interpreter, SMOP_REFERENCE(interpreter,node),NULL,NULL));
        SMOP_DISPATCH(interpreter,SMOP_RI(back),SMOP__ID__setr,
                      SMOP__NATIVE__capture_create(interpreter,back, (SMOP__Object*[]){r,NULL}, NULL));
        SMOP_DISPATCH(interpreter,SMOP_RI(interpreter),SMOP__ID__goto,
                      SMOP__NATIVE__capture_create(interpreter, SMOP_REFERENCE(interpreter,back), NULL, NULL));
        SMOP_RELEASE(interpreter, frame);
        ret = SMOP__NATIVE__bool_true;
      } else {
        smop_lowlevel_unlock(frame);
        ret = SMOP__NATIVE__bool_false;
      }
      SMOP_RELEASE(interpreter,frame);
    } else {
      ret = SMOP__NATIVE__bool_false;
    }    
  
  } else if (identifier ==  SMOP__ID__back) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      smop_lowlevel_rdlock(frame);
      SMOP__Object* back = ((smop_slime_frame_struct*)frame)->back;
      smop_lowlevel_unlock(frame);
      ret = SMOP_REFERENCE(interpreter, back);
      SMOP_RELEASE(interpreter,frame);
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier ==SMOP__ID__lexical) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      smop_lowlevel_rdlock(frame);
      SMOP__Object* lexical = ((smop_slime_frame_struct*)frame)->lexical;
      smop_lowlevel_unlock(frame);
      ret = SMOP_REFERENCE(interpreter, lexical);
      SMOP_RELEASE(interpreter,frame);
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier ==  SMOP__ID__forget) {
    smop_slime_frame_struct* frame = (smop_slime_frame_struct*)SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && (SMOP__Object*)frame != SMOP__SLIME__Frame) {
      smop_lowlevel_wrlock((SMOP__Object*)frame);
      int pc = frame->pc;
      SMOP__Object** mynodes = frame->nodes;
      smop_lowlevel_unlock((SMOP__Object*)frame);
      int i;
      for (i = 0; i < pc; i++) {
        SMOP_RELEASE(interpreter,mynodes[i]);
        mynodes[i] = SMOP__NATIVE__bool_false;
      }
      SMOP_RELEASE(interpreter,frame);
    }
    ret = SMOP__NATIVE__bool_false;
  
  } else if (identifier ==SMOP__ID__copy) {

    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      SMOP__Object* count = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
      assert(SMOP_RI(count) == (SMOP__ResponderInterface*)SMOP__NATIVE__int);
      int c = SMOP__NATIVE__int_fetch(count);
      SMOP_RELEASE(interpreter, count);
      smop_lowlevel_rdlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      SMOP__Object* node = ((smop_slime_frame_struct*)frame)->nodes[pc - c];
      smop_lowlevel_unlock(frame);
      ret = SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__result,
                          SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node),NULL,NULL));
      SMOP_RELEASE(interpreter,frame);
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier ==  SMOP__ID__move_responder) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      SMOP__Object* count = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
      SMOP__Object* target = SMOP__NATIVE__capture_positional(interpreter, capture, 1);
      assert(SMOP_RI(count) == (SMOP__ResponderInterface*)SMOP__NATIVE__int);
      assert(SMOP_RI(target) == (SMOP__ResponderInterface*)SMOP__NATIVE__int);
      int c = SMOP__NATIVE__int_fetch(count);
      int t = SMOP__NATIVE__int_fetch(target);
      SMOP_RELEASE(interpreter, count);
      SMOP_RELEASE(interpreter, target);
      smop_lowlevel_rdlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      SMOP__Object* node = ((smop_slime_frame_struct*)frame)->nodes[pc - c];
      SMOP__Object* thisnode = ((smop_slime_frame_struct*)frame)->nodes[pc + t];
      smop_lowlevel_unlock(frame);
      SMOP__Object* res = SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__result,
                                        SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node),NULL,NULL));
      SMOP_DISPATCH(interpreter,SMOP_RI(thisnode),SMOP__ID__responder,
                    SMOP__NATIVE__capture_create(interpreter,
                                                 SMOP_REFERENCE(interpreter,thisnode),
                                                 (SMOP__Object*[]){(SMOP__Object*)SMOP_RI(res),NULL},NULL));
      SMOP_RELEASE(interpreter, res);
      ret = SMOP__NATIVE__bool_true;
      SMOP_RELEASE(interpreter,frame);
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier ==  SMOP__ID__goto) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      SMOP__Object* count = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
      assert(SMOP_RI(count) == (SMOP__ResponderInterface*)SMOP__NATIVE__int);
      int c = SMOP__NATIVE__int_fetch(count);
      smop_lowlevel_wrlock(frame);
      ((smop_slime_frame_struct*)frame)->pc += c;
      smop_lowlevel_unlock(frame);
      ret = SMOP__NATIVE__bool_true;
      SMOP_RELEASE(interpreter,count);
      SMOP_RELEASE(interpreter,frame);
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier ==  SMOP__ID__move_identifier) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      SMOP__Object* count = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
      SMOP__Object* target = SMOP__NATIVE__capture_positional(interpreter, capture, 1);
      assert(SMOP_RI(count) == (SMOP__ResponderInterface*)SMOP__NATIVE__int);
      assert(SMOP_RI(target) == (SMOP__ResponderInterface*)SMOP__NATIVE__int);
      int c = SMOP__NATIVE__int_fetch(count);
      int t = SMOP__NATIVE__int_fetch(target);
      smop_lowlevel_rdlock(frame);
      int pc = ((smop_slime_frame_struct*)frame)->pc;
      SMOP__Object* node = ((smop_slime_frame_struct*)frame)->nodes[pc - c];
      SMOP__Object* thisnode = ((smop_slime_frame_struct*)frame)->nodes[pc + t];
      smop_lowlevel_unlock(frame);
      SMOP__Object* res = SMOP_DISPATCH(interpreter,SMOP_RI(node),SMOP__ID__result,
                                        SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node),NULL,NULL));
      SMOP_DISPATCH(interpreter,SMOP_RI(thisnode),SMOP__ID__identifier,
                    SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,thisnode),(SMOP__Object*[]){res,NULL},NULL));
      SMOP_RELEASE(interpreter,count);
      SMOP_RELEASE(interpreter,target);
      SMOP_RELEASE(interpreter,frame);
      ret = SMOP__NATIVE__bool_true;
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier ==SMOP__ID__move_capturize) {
    SMOP__Object* frame = SMOP__NATIVE__capture_invocant(interpreter, capture);
    if (frame && frame != SMOP__SLIME__Frame) {
      SMOP__Object* capturize = SMOP__NATIVE__capture_positional(interpreter, capture, 0);
      assert(SMOP_RI(capturize) == (SMOP__ResponderInterface*)SMOP__SLIME__Capturize);

      int i_invocant = SMOP__SLIME__Capturize_invocant(capturize);
      int n_pos = 0;
      int* i_positional = SMOP__SLIME__Capturize_positional(capturize, &n_pos);
      int n_nam = 0;
      int* i_named = SMOP__SLIME__Capturize_named(capturize, &n_nam);
      int t = SMOP__SLIME__Capturize_target(capturize);

      SMOP_RELEASE(interpreter,capturize); capturize = NULL;

      SMOP__Object** nodes_positional_arr = malloc(sizeof(SMOP__Object*) * n_pos);
      assert(nodes_positional_arr);
      SMOP__Object** nodes_named_arr = malloc(sizeof(SMOP__Object*) * n_nam);
      assert(nodes_named_arr);

      smop_lowlevel_rdlock(frame);

      int pc = ((smop_slime_frame_struct*)frame)->pc;

      SMOP__Object* node_invocant = ((smop_slime_frame_struct*)frame)->nodes[pc - i_invocant];
      int i;
      for (i = 0; i < n_pos; i++) {
        nodes_positional_arr[i] = ((smop_slime_frame_struct*)frame)->nodes[pc - i_positional[i]];
      }
      for (i = 0; i < n_nam; i++) {
        nodes_named_arr[i] = ((smop_slime_frame_struct*)frame)->nodes[pc - i_named[i]];
      }

      SMOP__Object* targetnode = ((smop_slime_frame_struct*)frame)->nodes[pc + t];

      smop_lowlevel_unlock(frame);

      free(i_positional); free(i_named);

      SMOP__Object** result_positional_arr = calloc(1,sizeof(SMOP__Object*) * (n_pos + 1));
      assert(result_positional_arr);
      SMOP__Object** result_named_arr = calloc(1,sizeof(SMOP__Object*) * (n_nam + 1));
      assert(result_named_arr);

      SMOP__Object* result_invocant =
        SMOP_DISPATCH(interpreter,SMOP_RI(node_invocant),
                      SMOP__ID__result,
                      SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node_invocant),NULL,NULL));
    
      for (i = 0; i < n_pos; i++) {
        result_positional_arr[i] =
          SMOP_DISPATCH(interpreter,SMOP_RI(nodes_positional_arr[i]),
                        SMOP__ID__result,SMOP__NATIVE__capture_create(interpreter,
                                                                      SMOP_REFERENCE(interpreter,nodes_positional_arr[i]),
                                                                      NULL,NULL));
      }
      for (i = 0; i < n_nam; i++) {
        result_named_arr[i] = 
          SMOP_DISPATCH(interpreter,SMOP_RI(nodes_named_arr[i]),
                        SMOP__ID__result,SMOP__NATIVE__capture_create(interpreter,
                                                                      SMOP_REFERENCE(interpreter,nodes_named_arr[i]),
                                                                      NULL,NULL));
      }
      
      SMOP_RELEASE(interpreter,
                   SMOP_DISPATCH(interpreter,SMOP_RI(node_invocant),
                    SMOP__ID__result,
                    SMOP__NATIVE__capture_create(interpreter,SMOP_REFERENCE(interpreter,node_invocant),
                                                 (SMOP__Object*[]){SMOP__NATIVE__bool_false,NULL},NULL)));
      for (i = 0; i < n_pos; i++) {
        SMOP_RELEASE(interpreter,
                     SMOP_DISPATCH(interpreter,SMOP_RI(nodes_positional_arr[i]),
                                   SMOP__ID__result,SMOP__NATIVE__capture_create(interpreter,
                                                                                 SMOP_REFERENCE(interpreter,nodes_positional_arr[i]),
                                                                                 (SMOP__Object*[]){SMOP__NATIVE__bool_false,NULL},NULL)));
      }
      for (i = 0; i < n_nam; i++) {
        SMOP_RELEASE(interpreter,
                     SMOP_DISPATCH(interpreter,SMOP_RI(nodes_named_arr[i]),
                                   SMOP__ID__result,SMOP__NATIVE__capture_create(interpreter,
                                                                                 SMOP_REFERENCE(interpreter,nodes_named_arr[i]),
                                                                                 (SMOP__Object*[]){SMOP__NATIVE__bool_false,NULL},NULL)));
      }


      SMOP__Object* res = SMOP__NATIVE__capture_create(interpreter,result_invocant, result_positional_arr, result_named_arr);

      free(nodes_named_arr);
      free(nodes_positional_arr);
      free(result_named_arr);
      free(result_positional_arr);

      SMOP_RELEASE(interpreter,
                   SMOP_DISPATCH(interpreter,SMOP_RI(targetnode),SMOP__ID__capture,
                                 SMOP__NATIVE__capture_create(interpreter,
                                                              SMOP_REFERENCE(interpreter,targetnode),
                                                              (SMOP__Object*[]){res,NULL},NULL)));

      SMOP_RELEASE(interpreter,frame);
      ret = SMOP__NATIVE__bool_true;
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  } else if (identifier ==  SMOP__ID__DESTROYALL) {
    smop_slime_frame_struct* frame = (smop_slime_frame_struct*)capture;
    if (frame && (SMOP__Object*)frame != SMOP__SLIME__Frame) {
      smop_lowlevel_wrlock((SMOP__Object*)frame);
      SMOP__Object* lexical = frame->lexical; frame->lexical = NULL;
      SMOP__Object** nodes = frame->nodes; frame->nodes = NULL;
      int ncount = frame->node_count; frame->node_count = 0;
      SMOP__Object* back = frame->back; frame->back = NULL;
      smop_lowlevel_unlock((SMOP__Object*)frame);

      if (lexical) SMOP_RELEASE(interpreter,lexical);
      if (back) SMOP_RELEASE(interpreter,back);
      int i;
      for (i = 0; i < ncount; i++)
        SMOP_RELEASE(interpreter, nodes[i]);
      
      if (nodes) free(nodes);

      ret = SMOP__NATIVE__bool_false;
    } else {
      ret = SMOP__NATIVE__bool_false;
    }
  
  }
  SMOP_RELEASE(interpreter,capture);
  return ret;
}

static SMOP__Object* frame_reference(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if (responder != (SMOP__ResponderInterface*)obj) {
    smop_lowlevel_refcnt_inc(interpreter, responder, obj);
  }
  return obj;
}

static SMOP__Object* frame_release(SMOP__Object* interpreter, SMOP__ResponderInterface* responder, SMOP__Object* obj) {
  if (responder != (SMOP__ResponderInterface*)obj) {
    smop_lowlevel_refcnt_dec(interpreter, responder, obj);
  }
  return obj;
}

void smop_slime_frame_init() {
  SMOP__SLIME__Frame = calloc(1, sizeof(SMOP__ResponderInterface));
  assert(SMOP__SLIME__Frame);
  ((SMOP__ResponderInterface*)SMOP__SLIME__Frame)->MESSAGE = frame_message;
  ((SMOP__ResponderInterface*)SMOP__SLIME__Frame)->REFERENCE = frame_reference;
  ((SMOP__ResponderInterface*)SMOP__SLIME__Frame)->RELEASE = frame_release;
  ((SMOP__ResponderInterface*)SMOP__SLIME__Frame)->id = "SMOP SLIME Frame";
}

void smop_slime_frame_destr() {
  free(SMOP__SLIME__Frame);
}
