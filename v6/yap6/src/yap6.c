#include "yap6.h"
#include <stdlib.h>
#include <assert.h>

void yap6_init() {

  yap6_ident_dispatcher_init();
  yap6_const_init();
  yap6_int_dispatcher_init();
  yap6_list_dispatcher_init();

}
