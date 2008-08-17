cmake_minimum_required(VERSION 2.6)
include_directories (../include)
foreach(sm0p_file
    smop_lowlevel.sm0p
    smop_s1p_lexicalscope.sm0p
    smop_s1p_capturize.c
    smop_s1p_defaultblocksignature.sm0p
    p6opaque.sm0p
    smop_s1p_attribute.sm0p
)
    string(REGEX REPLACE ".sm0p$" ".c" c_file ${sm0p_file})
    add_custom_command (OUTPUT ${c_file} COMMAND perl ARGS ../sm0p.pl `pwd`/.. ${sm0p_file} ${c_file})
endforeach()
INCLUDE (FindThreads)
add_library (smop
    idconst.c
    lowlevel_method.c
    native_bool.c
    native_capture.c
    native_int.c
    native_uint.c
    p6opaque.c
    smop.c
    smop_internal.h
    smop_interpreter.c
    smop_lowlevel.c
    smop_mold.c
    smop_s1p_array.c
    smop_s1p_attribute.c
    smop_s1p_capturize.c
    smop_s1p_ccode.c
    smop_s1p_code.c
    smop_s1p_defaultblocksignature.c
    smop_s1p_hash.c
    smop_s1p_io.c
    smop_s1p_lexicalscope.c
    smop_s1p_root_namespace.c
    smop_s1p_scalar.c
    smop_s1p_str.c
    smop_slime_capturize.c
    smop_slime_currentframe.c
    smop_slime_frame.c
    smop_slime_node.c
)

