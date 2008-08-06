#include <stdlib.h>
#include <stdio.h>
#include <smop.h>
#include <smop_lowlevel.h>
#include <smop_s1p.h>

SMOP__Object* SMOP__S1P__IO;

typedef struct smop_s1p_io_struct {
  SMOP__Object__BASE
} smop_s1p_io_struct;

SMOP__Object* SMOP__S1P__IO_create(void) {
    SMOP__Object* ret = smop_lowlevel_alloc(sizeof(smop_s1p_io_struct));
    ret->RI = (SMOP__ResponderInterface*)SMOP__S1P__IO;
    return ret;
}

static SMOP__Object* smop_s1p_io_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {

  smop_s1p_io_struct* invocant;
  invocant = (smop_s1p_io_struct*)(SMOP__NATIVE__capture_invocant(interpreter, capture));

  static SMOP__Object* ID_print = NULL;

  if (!ID_print) ID_print = SMOP__NATIVE__idconst_create("print");

  if (identifier == ID_print) {
    int pc = SMOP__NATIVE__capture_positional_count(interpreter,capture);
    int i;
    for (i=0;i<pc;i++) {
        SMOP__Object* obj = SMOP__NATIVE__capture_positional(interpreter,capture,i);
        if (SMOP_RI(obj) == (SMOP__ResponderInterface*)SMOP__S1P__Str) {
            printf("%s",SMOP__S1P__Str_c_str(obj));
        } else if (SMOP_RI(obj) == (SMOP__ResponderInterface*)SMOP__NATIVE__int){
            printf("%d",SMOP__NATIVE__int_fetch(obj));
        } else if (SMOP_RI(obj) == SMOP_RI(SMOP__ID__new)) {
            int len;
            printf("%s",SMOP__NATIVE__idconst_fetch(obj,&len));
        } else {
            fprintf(stderr,"unsupported object passed to S1P::IO.print\n");
        }

        SMOP_RELEASE(interpreter,obj);
    }
  } else if (identifier == SMOP__ID__new) {
    SMOP__S1P__IO_create();
  } else if (identifier == SMOP__ID__DESTROYALL) {
  } else {
    fprintf(stderr,"unkown method at S1P::IO\n");
  }

  SMOP_RELEASE(interpreter,invocant);
  SMOP_RELEASE(interpreter,capture);
  return SMOP__NATIVE__bool_false;
}


void smop_s1p_io_init() {
  SMOP__S1P__IO = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__IO)->MESSAGE = smop_s1p_io_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__IO)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__IO)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__IO)->id = "Lowlevel io";
}

void smop_s1p_io_destr() {
  free(SMOP__S1P__IO);
}


