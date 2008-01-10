#include "smop.h"
#include <stdlib.h>
#include <assert.h>

void smop_init() {

  smop_memory_init();

  // basic dispatch
  smop_ident_dispatcher_init();
  smop_const_init();
  smop_int_dispatcher_init();
  smop_scalar_dispatcher_init();
  smop_list_dispatcher_init();
  smop_pair_dispatcher_init();
  smop_bytes_dispatcher_init();

  // which implementation
  smop_int_dispatcher_which_init();
  smop_ident_dispatcher_which_init();
}

void smop_destr() {

  smop_bytes_dispatcher_destr();
  smop_pair_dispatcher_destr();
  smop_list_dispatcher_destr();
  smop_scalar_dispatcher_destr();
  smop_int_dispatcher_destr();
  smop_const_destr();
  smop_ident_dispatcher_destr();

  smop_memory_destr();

}
