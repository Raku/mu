#ifndef SMOP_INTERNAL_H
#define SMOP_INTERNAL_H

#include <smop_lowlevel.h>

/* This is a private header file to be used by the smop internals
 */

#define SMOP_INTERNAL_INIT_SEQUENCE   \
     smop_idconst_init();             \
     smop_lowlevel_init();            \
     smop_native_capture_init();      \
     smop_native_bool_init();         \
     smop_interpreter_init();         \
     smop_slime_frame_init();         \
     smop_slime_node_init();          \
     smop_slime_operators_init();

#define SMOP_INTERNAL_DESTR_SEQUENCE  \
     smop_slime_operators_destr();    \
     smop_slime_node_destr();         \
     smop_slime_frame_destr();        \
     smop_interpreter_destr();        \
     smop_native_bool_destr();        \     
     smop_native_capture_destr();     \
     smop_lowlevel_destroy();         \
     smop_idconst_destr();

void smop_idconst_init();
void smop_idconst_destr();
void smop_native_capture_init();
void smop_native_capture_destr();
void smop_interpreter_init();
void smop_interpreter_destr(); 
void smop_slime_frame_init();
void smop_slime_frame_destr();
void smop_slime_node_init();
void smop_slime_node_destr();
void smop_slime_operators_init();
void smop_slime_operators_destr();
void smop_native_bool_init();
void smop_native_bool_destr();

#endif
