#include <stdlib.h>
#include <assert.h>
#include "yap6.h"

YAP6__CORE__Value* yap6_const_undef;
YAP6__CORE__Value* yap6_const_true;
YAP6__CORE__Value* yap6_const_false;

void yap6_const_init() {
  yap6_const_undef = calloc(1,sizeof(YAP6__CORE__Value));
  assert(yap6_const_undef);
  yap6_const_undef->dispatcher = yap6_const_ident_dispatcher;

  yap6_const_true = calloc(1,sizeof(YAP6__CORE__Value));
  assert(yap6_const_true);
  yap6_const_true->dispatcher = yap6_const_ident_dispatcher;

  yap6_const_false = calloc(1,sizeof(YAP6__CORE__Value));
  assert(yap6_const_false);
  yap6_const_false->dispatcher = yap6_const_ident_dispatcher;
}
