#include <smop.h>
#include "smop_internal.h"

void smop_init() {
  SMOP_INTERNAL_BOOT_SEQUENCE;
  SMOP_INTERNAL_INIT_SEQUENCE;
}

void smop_destr() {
  SMOP_INTERNAL_DESTROY_SEQUENCE;
  SMOP_INTERNAL_SHUTDOWN_SEQUENCE;
}
