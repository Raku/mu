#include "vroom.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


static void ident_dispatcher_DESTR(VROOM__CORE__Dispatcher* self,
                                          VROOM__CORE__Value* value) {
}

static VROOM__CORE__bytes* ident_dispatcher_WHICH(VROOM__CORE__Dispatcher* self,
                                   VROOM__CORE__Value* value) {
  char str[32];
  sprintf(str, "value:%p", value);
  int len = strlen(str);
  return vroom_bytes_create(str, len);

}

VROOM__CORE__Dispatcher* vroom_const_ident_dispatcher;

void vroom_ident_dispatcher_init() {
  vroom_const_ident_dispatcher = (VROOM__CORE__Dispatcher*)vroom_value_alloc(sizeof(VROOM__CORE__Dispatcher));
  vroom_const_ident_dispatcher->DESTR = &ident_dispatcher_DESTR;
}

void vroom_ident_dispatcher_destr() {
  vroom_value_refcnt_dec((VROOM__CORE__Value*)vroom_const_ident_dispatcher);
}

void vroom_ident_dispatcher_which_init() {
  vroom_const_ident_dispatcher->WHICH = &ident_dispatcher_WHICH;
}
