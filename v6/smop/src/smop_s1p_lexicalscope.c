#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <smop.h>
#include <smop_s1p.h>
#include <smop_oo.h>
#include <smop_lowlevel.h>

SMOP__Object* SMOP__S1P__LexicalScope;
typedef struct smop_s1p_lexicalscope_struct {
  SMOP__Object__BASE
  SMOP__Object* entries;
  SMOP__Object* outer;
} smop_s1p_lexicalscope_struct;

static SMOP__Object* SMOP__ID__entries;
static SMOP__Object* SMOP__ID__lookup;

static SMOP__Object* lexicalscope_message(SMOP__Object* interpreter,
                                          SMOP__ResponderInterface* self,
                                          SMOP__Object* identifier,
                                          SMOP__Object* capture) {

  SMOP__Object* ret = SMOP__NATIVE__bool_false;
  ___NATIVE_CAPTURE_ONLY___;
  ___CONST_IDENTIFIER_ONLY___;
  ___INVOCANT_RI_SHOULD_MATCH___;


  if (identifier == SMOP__ID__new) {
    ret = smop_lowlevel_alloc(sizeof(smop_s1p_lexicalscope_struct));
    ret->RI = (SMOP__ResponderInterface*)SMOP__S1P__LexicalScope;
    ((smop_s1p_lexicalscope_struct*)ret)->entries = SMOP__S1P__Hash_create();
    ((smop_s1p_lexicalscope_struct*)ret)->outer = SMOP__S1P__Scalar_create(SMOP__NATIVE__bool_false);

  } else if (identifier == SMOP__ID__outer) {
    smop_lowlevel_rdlock(invocant);
    ret = ((smop_s1p_lexicalscope_struct*)invocant)->outer;
    smop_lowlevel_unlock(invocant);
    SMOP_REFERENCE(interpreter, ret);

  } else if (identifier == SMOP__ID__entries) {
    smop_lowlevel_rdlock(invocant);
    ret = ((smop_s1p_lexicalscope_struct*)invocant)->entries;
    smop_lowlevel_unlock(invocant);
    SMOP_REFERENCE(interpreter, ret);

  } else if (identifier == SMOP__ID__postcircumfix_curly) {
    smop_lowlevel_rdlock(invocant);
    SMOP__Object* entries = ((smop_s1p_lexicalscope_struct*)invocant)->entries;
    smop_lowlevel_unlock(invocant);
    ret = SMOP_DISPATCH(interpreter, SMOP_RI(entries),
                        SMOP__ID__postcircumfix_curly,
                        SMOP__NATIVE__capture_delegate(interpreter,
                                                       SMOP_REFERENCE(interpreter,entries),
                                                       SMOP_REFERENCE(interpreter,capture)));

  } else if (identifier == SMOP__ID__lookup) {
    printf("TODO: smop_s1p_lexicalscope lookup!\n");
    abort();

  } else {
    ___UNKNOWN_METHOD___;

  }

  SMOP_RELEASE(interpreter, invocant);
  SMOP_RELEASE(interpreter, capture);
  return ret;
}

void smop_s1p_lexicalscope_init() {

  SMOP__S1P__LexicalScope = calloc(1,sizeof(SMOP__ResponderInterface));
  ((SMOP__ResponderInterface*)SMOP__S1P__LexicalScope)->MESSAGE = lexicalscope_message;
  ((SMOP__ResponderInterface*)SMOP__S1P__LexicalScope)->REFERENCE = smop_lowlevel_generic_reference;
  ((SMOP__ResponderInterface*)SMOP__S1P__LexicalScope)->RELEASE = smop_lowlevel_generic_release;
  ((SMOP__ResponderInterface*)SMOP__S1P__LexicalScope)->id = "S1P LexicalScope";

  SMOP__ID__entries = SMOP__NATIVE__idconst_create("entries");
  SMOP__ID__lookup = SMOP__NATIVE__idconst_create("lookup");

}

void smop_s1p_lexicalscope_destr() {
  free(SMOP__S1P__LexicalScope);
}
