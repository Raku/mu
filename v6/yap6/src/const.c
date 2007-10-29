#include <stdlib.h>
#include <assert.h>
#include "yap6.h"

YAP6__CORE__Value* yap6_const_undef;
YAP6__CORE__bool* yap6_const_true;
YAP6__CORE__bool* yap6_const_false;

void yap6_const_init() {

  yap6_const_undef = yap6_value_alloc(sizeof(YAP6__CORE__Value));
  yap6_const_undef->dispatcher = yap6_const_ident_dispatcher;
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_ident_dispatcher);

  yap6_const_true = (YAP6__CORE__bool*)yap6_value_alloc(sizeof(YAP6__CORE__Value));
  yap6_const_true->dispatcher = yap6_const_ident_dispatcher;
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_ident_dispatcher);

  yap6_const_false = (YAP6__CORE__bool*)yap6_value_alloc(sizeof(YAP6__CORE__Value));
  yap6_const_false->dispatcher = yap6_const_ident_dispatcher;
  yap6_value_refcnt_inc((YAP6__CORE__Value*)yap6_const_ident_dispatcher);
}

void yap6_const_destr() {
  yap6_value_refcnt_dec(yap6_const_false);
  yap6_value_refcnt_dec(yap6_const_true);
  yap6_value_refcnt_dec(yap6_const_undef);
}
