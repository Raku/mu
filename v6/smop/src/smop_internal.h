#ifndef SMOP_INTERNAL_H
#define SMOP_INTERNAL_H

#include <smop_lowlevel.h>

/* This is a private header file to be used by the smop internals
 */


/*
 * This is the boot sequence of the runtime bootstrap. Only the
 * absolutely needed things must be here.
 *
 * See http://www.perlfoundation.org/perl6/index.cgi?smop_boot_sequence
 */
#define SMOP_INTERNAL_BOOT_SEQUENCE   \
     smop_idconst_init();             \
     smop_lowlevel_init();            \
     smop_interpreter_init();         \
     smop_native_capture_init();      \
     smop_native_int_init();          \
     smop_native_bool_init();         \
     smop_slime_frame_init();         \
     smop_slime_currentframe_init();  \
     smop_slime_node_init();          \
     smop_slime_capturize_init();
#define SMOP_INTERNAL_SHUTDOWN_SEQUENCE  \
     smop_slime_capturize_destr();       \
     smop_slime_node_destr();            \
     smop_slime_currentframe_destr();    \
     smop_slime_frame_destr();           \
     smop_native_int_destr();            \
     smop_native_bool_destr();           \
     smop_native_capture_destr();        \
     smop_interpreter_destr();           \
     smop_lowlevel_destr();              \
     smop_idconst_destr();

/*
 * This is the initialization sequence for other built-in types that
 * are not required to bootstrap the runtime
 *
 * See http://www.perlfoundation.org/perl6/index.cgi?smop_boot_sequence
 */
#define SMOP_INTERNAL_INIT_SEQUENCE         \
     smop_s1p_code_init();                  \
     smop_s1p_ccode_init();                 \
     smop_lowlevel_method_init();           \
     smop_p6opaque_init();                  \
     smop_native_uint_init();               \
     smop_s1p_scalar_init();                \
     smop_s1p_str_init();                   \
     smop_s1p_array_init();                 \
     smop_s1p_hash_init();                  \
     smop_s1p_io_init();                    \
     smop_s1p_attribute_init();                 
#define SMOP_INTERNAL_DESTROY_SEQUENCE      \
     smop_s1p_attribute_destr();            \
     smop_s1p_io_destr();                   \
     smop_s1p_hash_destr();                 \
     smop_s1p_array_destr();                \
     smop_s1p_str_destr();                  \
     smop_s1p_scalar_destr();               \
     smop_native_uint_destr();              \
     smop_p6opaque_destr();                 \
     smop_lowlevel_method_destr();          \
     smop_s1p_ccode_destr();                \
     smop_s1p_code_destr();

#define SMOP_BOOTSTRAP_INIT_SEQUENCE        \
     smop_s1p_root_namespace_init();        

#define SMOP_BOOTSTRAP_DESTR_SEQUENCE       \
     smop_s1p_root_namespace_destr();       

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
void smop_lowlevel_method_init();
void smop_lowlevel_method_destr();
void smop_p6opaque_init();
void smop_p6opaque_destr();
void smop_s1p_scalar_init();
void smop_s1p_scalar_destr();
void smop_s1p_hash_init();
void smop_s1p_hash_destr();
void smop_s1p_array_init();
void smop_s1p_array_destr();
void smop_s1p_ccode_init();
void smop_s1p_ccode_destr();

#endif
