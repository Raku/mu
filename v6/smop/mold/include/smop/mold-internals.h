#ifndef SMOP_Mold_Internals_H
#define SMOP_Mold_Internals_H

#include <smop/mold.h>

typedef struct smop_mold {
  SMOP__NAGC__Object__BASE
  int registers;
  SMOP__Object** constants;
  int constants_len;
  int *opcodes;
  int opcodes_len;
} smop_mold;

#endif
