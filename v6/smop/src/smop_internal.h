#ifndef SMOP_INTERNAL_H
#define SMOP_INTERNAL_H

#include <smop_lowlevel.h>

/* This is a private header file to be used by the smop internals
 */

#define SMOP_INTERNAL_INIT_SEQUENCE   \
     smop_idconst_init();             \
     smop_lowlevel_init();            \
     smop_interpreter_init();         \
     smop_native_capture_init();      \
     smop_native_int_init();          \
     smop_native_uint_init();         \
     smop_native_bool_init();         \
     smop_slime_frame_init();         \
     smop_slime_currentframe_init();  \
     smop_slime_node_init();          \
     smop_slime_capturize_init();

#define SMOP_INTERNAL_DESTR_SEQUENCE  \
     smop_slime_capturize_destr();    \
     smop_slime_node_destr();         \
     smop_slime_currentframe_destr(); \
     smop_slime_frame_destr();        \
     smop_native_int_destr();         \
     smop_native_uint_destr();        \
     smop_native_bool_destr();        \
     smop_native_capture_destr();     \
     smop_interpreter_destr();        \
     smop_lowlevel_destr();           \
     smop_idconst_destr();

void smop_idconst_init();
void smop_idconst_destr();
void smop_native_capture_init();
void smop_native_capture_destr();
void smop_interpreter_init();
void smop_interpreter_destr(); 
void smop_slime_frame_init();
void smop_slime_frame_destr();
void smop_slime_currentframe_init();
void smop_slime_currentframe_destr();
void smop_slime_node_init();
void smop_slime_node_destr();
void smop_slime_capturize_init();
void smop_slime_capturize_destr();
void smop_native_bool_init();
void smop_native_bool_destr();
void smop_native_int_init();
void smop_native_int_destr();
void smop_native_uint_init();
void smop_native_uint_destr();

#endif
