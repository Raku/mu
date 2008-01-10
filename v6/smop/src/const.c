#include <stdlib.h>
#include <assert.h>
#include "vroom.h"

VROOM__CORE__Value* vroom_const_undef;
VROOM__CORE__Value* vroom_bool_false;

void vroom_const_init() {

  vroom_const_undef = vroom_value_alloc(sizeof(VROOM__CORE__Value));
  vroom_const_undef->dispatcher = vroom_const_ident_dispatcher;
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_ident_dispatcher);

  vroom_bool_false = vroom_value_alloc(sizeof(VROOM__CORE__Value));
  vroom_bool_false->dispatcher = vroom_const_ident_dispatcher;
  vroom_value_refcnt_inc((VROOM__CORE__Value*)vroom_const_ident_dispatcher);
}

void vroom_const_destr() {
  vroom_value_refcnt_dec(vroom_bool_false);
  vroom_value_refcnt_dec(vroom_const_undef);
}
