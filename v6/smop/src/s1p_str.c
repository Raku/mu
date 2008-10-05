#include <stdlib.h>
#include <stdio.h>
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
    ret->str = malloc(sizeof(char) * len);
    strncpy(ret->str,str,len);
    ret->len  = len;
    ret->RI = (SMOP__ResponderInterface*)SMOP__S1P__Str;
    return (SMOP__Object*) ret;
}
SMOP__Object* SMOP__S1P__Str_create(char *data) {
    return SMOP__S1P__Str_createn(data,strlen(data)+1);
}


char* SMOP__S1P__Str_c_str(SMOP__Object* obj) {
    return ((smop_s1p_str_struct*) obj)->str;
}

static SMOP__Object* smop_s1p_str_message(SMOP__Object* interpreter,
                                     SMOP__ResponderInterface* self,
                                     SMOP__Object* identifier,
                                     SMOP__Object* capture) {

  smop_s1p_str_struct *invocant = (smop_s1p_str_struct*)(SMOP__NATIVE__capture_invocant(interpreter, capture));

  if (invocant) SMOP_RELEASE(interpreter,invocant);
  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  if (identifier == SMOP__ID__DESTROYALL) {
      free(invocant->str);
  } else if (identifier == SMOP__ID__perl) {
      char *perl = malloc(sizeof(char) * (invocant->len+2));
      perl[0] = '"';
      int i;for (i=0;i<invocant->len;i++) perl[i+1] = invocant->str[i];
      perl[invocant->len+1] = '"';
      ret = SMOP__S1P__Str_createn(perl,invocant->len+2);
      free(perl);
  } else if (identifier == SMOP__ID__infix_eq) {
      SMOP__Object* other = SMOP__NATIVE__capture_positional(interpreter,capture,0);
      SMOP_RELEASE(interpreter,capture);
      if (SMOP_RI(other) == (SMOP__ResponderInterface*)SMOP__S1P__Str) {
          smop_s1p_str_struct *other_str = (smop_s1p_str_struct*) other;
          if (other_str->len == invocant->len && strncmp(other_str->str,invocant->str,other_str->len) == 0) {
            ret = SMOP__NATIVE__bool_true;
          } else {
            ret = SMOP__NATIVE__bool_false;
          }
          SMOP_RELEASE(interpreter,other);
      } else {
        fprintf(stderr,"eq is supported on two S1P Str's for now, sorry\n");
      }
  }

  SMOP_RELEASE(interpreter,capture);
  return ret;
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


