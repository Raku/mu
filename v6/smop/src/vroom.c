#include "vroom.h"
#include <stdlib.h>
#include <assert.h>

void vroom_init() {

  vroom_memory_init();

  // basic dispatch
  vroom_ident_dispatcher_init();
  vroom_const_init();
  vroom_int_dispatcher_init();
  vroom_scalar_dispatcher_init();
  vroom_list_dispatcher_init();
  vroom_pair_dispatcher_init();
  vroom_bytes_dispatcher_init();

  // which implementation
  vroom_int_dispatcher_which_init();
  vroom_ident_dispatcher_which_init();
}

void vroom_destr() {

  vroom_bytes_dispatcher_destr();
  vroom_pair_dispatcher_destr();
  vroom_list_dispatcher_destr();
  vroom_scalar_dispatcher_destr();
  vroom_int_dispatcher_destr();
  vroom_const_destr();
  vroom_ident_dispatcher_destr();

  vroom_memory_destr();

}
