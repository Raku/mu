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
     smop_ri_init();                  \
     smop_proto_init();               
#define SMOP_INTERNAL_SHUTDOWN_SEQUENCE  \
     smop_native_int_destr();            \
     smop_native_bool_destr();           \
     smop_native_capture_destr();        \
     smop_proto_destr();                 \
     smop_ri_destr();                    \
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
     smop_mold_init();                      \
     smop_s1p_lexicalscope_init();          \
     smop_s1p_package_init();               \
     smop_s1p_hash_bvalue_init();           \
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
     smop_s1p_attribute_init();             \
     smop_s1p_capturize_init();             \
     smop_s1p_pureprototypehow_init();      \
     smop_s1p_defaultblocksignature_init(); \
     smop_s1p_bindcapturesignature_init();  \
     smop_s1p_adhocsignature_init();        \
     smop_s1p_map_init();                   \
     smop_s1p_grep_init();                  \
     smop_s1p_array_iterator_init();        \
     smop_s1p_emptylist_init();             \
     smop_s1p_endofiterator_init();         \
     smop_s1p_multi_init();                 \
     smop_pcl_coro_init();                  \
     smop_p5interpreter_init();             \
     smop_p5_sv_init();                     \

#define SMOP_INTERNAL_DESTROY_SEQUENCE      \
     smop_s1p_multi_destr();                \
     smop_s1p_endofiterator_destr();        \
     smop_s1p_emptylist_destr();            \
     smop_s1p_array_iterator_destr();       \
     smop_s1p_grep_destr();                 \
     smop_s1p_map_destr();                  \
     smop_s1p_bindcapturesignature_destr(); \
     smop_s1p_defaultblocksignature_destr();\
     smop_s1p_pureprototypehow_destr();     \
     smop_s1p_adhocsignature_destr();       \
     smop_s1p_capturize_destr();            \
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
     smop_s1p_code_destr();                 \
     smop_s1p_hash_bvalue_destr();          \
     smop_s1p_lexicalscope_destr();         \
     smop_s1p_package_destr();              \
     smop_mold_destr();                     \
     smop_pcl_coro_destr();                 \
     smop_p5interpreter_destr();            \
     smop_p5_sv_destr();                    \

#define SMOP_BOOTSTRAP_INIT_SEQUENCE        \
     smop_s1p_ritest_init();                \
     smop_s1p_attribute_mold_init();        \
     smop_s1p_hash_mold_init();      \
     smop_s1p_hash_bvalue_mold_init();      \
     smop_s1p_root_namespace_init();        \
     smop_s1p_lexical_prelude_init();       \
     smop_s1p_lexicalscope_mold_init();     \
     smop_p6opaque_mold_init();             \
     smop_s1p_multi_mold_init();            \
     smop_s1p_defaultblocksignature_mold_init(); \
     smop_s1p_bindcapturesignature_mold_init();  \
     smop_s1p_pureprototypehow_mold_init(); \
     smop_s1p_adhocsignature_mold_init();   \
     smop_s1p_code_mold_init();             \
     smop_s1p_map_mold_init();              \
     smop_s1p_grep_mold_init();             \
     smop_s1p_array_iterator_mold_init();   \

#define SMOP_BOOTSTRAP_DESTR_SEQUENCE       \
     smop_s1p_array_iterator_mold_destr();  \
     smop_s1p_grep_mold_destr();            \
     smop_s1p_map_mold_destr();             \
     smop_s1p_code_mold_destr();            \
     smop_s1p_adhocsignature_mold_destr();  \
     smop_s1p_pureprototypehow_mold_destr();\
     smop_s1p_bindcapturesignature_mold_destr(); \
     smop_s1p_defaultblocksignature_mold_destr();\
     smop_s1p_multi_mold_destr();           \
     smop_p6opaque_mold_destr();            \
     smop_s1p_lexicalscope_mold_destr();    \
     smop_s1p_lexical_prelude_destr();      \
     smop_s1p_root_namespace_destr();       \
     smop_s1p_hash_bvalue_mold_destr();     \
     smop_s1p_hash_mold_destr();     \
     smop_s1p_attribute_mold_destr();       \
     smop_s1p_ritest_destr();               \

void smop_idconst_init();
void smop_idconst_destr();
void smop_native_capture_init();
void smop_native_capture_destr();
void smop_interpreter_init();
void smop_interpreter_destr(); 
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
void smop_s1p_lexicalscope_init();
void smop_s1p_lexicalscope_destr();
void smop_s1p_adhocsignature_init();
void smop_s1p_adhocsignature_destr();
void smop_s1p_defaultblocksignature_init();
void smop_s1p_defaultblocksignature_destr();
void smop_s1p_defaultblocksignature_mold_init();
void smop_s1p_defaultblocksignature_mold_destr();
void smop_s1p_bindcapturesignature_init();
void smop_s1p_bindcapturesignature_destr();
void smop_s1p_bindcapturesignature_mold_init();
void smop_s1p_bindcapturesignature_mold_destr();
void smop_s1p_pureprototypehow_init();
void smop_s1p_pureprototypehow_destr();
void smop_s1p_capturize_init();
void smop_s1p_capturize_destr();
void smop_s1p_itemcontext_init();
void smop_s1p_itemcontext_destr();
void smop_s1p_itemrwcontext_init();
void smop_s1p_itemrwcontext_destr();
void smop_s1p_emptylist_init();
void smop_s1p_emptylist_destr();
void smop_s1p_endofiterator_init();
void smop_s1p_endofiterator_destr();
void smop_s1p_ritest_init();
void smop_s1p_ritest_destr();
void smop_s1p_multi_init();
void smop_s1p_multi_destr();

#endif
