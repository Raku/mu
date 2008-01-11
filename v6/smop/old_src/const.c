#include <stdlib.h>
#include <assert.h>
#include "smop.h"

SMOP__CORE__Value* smop_const_undef;
SMOP__CORE__Value* smop_bool_false;

void smop_const_init() {

  smop_const_undef = smop_value_alloc(sizeof(SMOP__CORE__Value));
  smop_const_undef->dispatcher = smop_const_ident_dispatcher;
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_ident_dispatcher);

  smop_bool_false = smop_value_alloc(sizeof(SMOP__CORE__Value));
  smop_bool_false->dispatcher = smop_const_ident_dispatcher;
  smop_value_refcnt_inc((SMOP__CORE__Value*)smop_const_ident_dispatcher);
}

void smop_const_destr() {
  smop_value_refcnt_dec(smop_bool_false);
  smop_value_refcnt_dec(smop_const_undef);
}
