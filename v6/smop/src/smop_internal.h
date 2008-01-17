#ifndef SMOP_INTERNAL_H
#define SMOP_INTERNAL_H

/* This is a private header file to be used by the smop internals
 */

#define SMOP_INTERNAL_INIT_SEQUENCE   \
     smop_idconst_init();

#define SMOP_INTERNAL_DESTR_SEQUENCE  \
     smop_idconst_destr();

// idconst
void smop_idconst_init();
void smop_idconst_destr();

#endif
