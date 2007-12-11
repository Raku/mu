#include "yap6.h"
#include <stdlib.h>
#include <assert.h>

void yap6_init() {

  yap6_ident_dispatcher_init();
  yap6_const_init();
  yap6_int_dispatcher_init();
  yap6_scalar_dispatcher_init();
  yap6_list_dispatcher_init();
  yap6_pair_dispatcher_init();
  yap6_bytes_dispatcher_init();
}

void yap6_destr() {

  yap6_bytes_dispatcher_destr();
  yap6_pair_dispatcher_destr();
  yap6_list_dispatcher_destr();
  yap6_scalar_dispatcher_destr();
  yap6_int_dispatcher_destr();
  yap6_const_destr();
  yap6_ident_dispatcher_destr();

}
