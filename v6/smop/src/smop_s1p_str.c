#include <stdlib.h>
#include <smop.h>
#include <string.h>
#include <smop_lowlevel.h>

SMOP__Object* SMOP__S1P__Str;

typedef struct smop_s1p_str_struct {
  SMOP__Object__BASE
  char* str;
  int len;
} smop_s1p_str_struct;

/* the str pointer is freed when the string is collected */
SMOP__Object* SMOP__S1P__Str_createn(char *str,int len) {
    smop_s1p_str_struct* ret = (smop_s1p_str_struct*) smop_lowlevel_alloc(sizeof(smop_s1p_str_struct));
    ret->str = str;
    ret->len  = len;
    ret->RI = (SMOP__ResponderInterface*)SMOP__S1P__Str;
    return (SMOP__Object*) ret;
}
SMOP__Object* SMOP__S1P__Str_create(char *data) {
    return SMOP__S1P__Str_createn(data,strlen(data));
}


char* SMOP__S1P__Str_c_str(SMOP__Object* obj) {
    return ((smop_s1p_str_struct*) obj)->str;
}

static SMOP__Object* smop_s1p_str_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {

  smop_s1p_str_struct* invocant = (smop_s1p_str_struct*)(SMOP__NATIVE__capture_invocant(interpreter, capture));

  if (invocant) SMOP_RELEASE(interpreter,invocant);

  if (identifier == SMOP__ID__DESTROYALL) {
      free(invocant->str);
  }

  SMOP_RELEASE(interpreter,capture);
  return SMOP__NATIVE__bool_false;
}


void smop_s1p_str_init() {
  SMOP__S1P__Str = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__Str)->MESSAGE = smop_s1p_str_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__Str)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__Str)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__Str)->id = "char* based Str";
}

void smop_s1p_str_destr() {
  free(SMOP__S1P__Str);
}


