project (smop)
cmake_minimum_required (VERSION 2.6)
include_directories (${smop_SOURCE_DIR}/include)

### m0ld

    add_custom_command (
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe
        WORKING_DIRECTORY ${smop_SOURCE_DIR}/m0ld
        COMMAND cabal ARGS configure
        COMMAND cabal ARGS build
        COMMAND cp ARGS ${smop_SOURCE_DIR}/m0ld/dist/build/m0ld/m0ld ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe
    )

### libsmop

add_custom_command (
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/src
    COMMAND mkdir ARGS ${CMAKE_CURRENT_BINARY_DIR}/src
)
foreach (sm0p_file
    src/smop_lowlevel.sm0p
    src/smop_s1p_lexicalscope.sm0p
    src/smop_s1p_defaultblocksignature.sm0p
    src/p6opaque.sm0p
    src/smop_s1p_attribute.sm0p
)
    string(REGEX REPLACE ".sm0p$" ".c" c_file ${sm0p_file})
    add_custom_command (
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        COMMAND perl ARGS ${smop_SOURCE_DIR}/sm0p.pl ${smop_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe ${smop_SOURCE_DIR}/${sm0p_file} ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/src
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe
        DEPENDS ${smop_SOURCE_DIR}/sm0p.pl
        DEPENDS ${smop_SOURCE_DIR}/${sm0p_file}
    )
endforeach()

INCLUDE (FindThreads)
add_library (smop
    src/idconst.c
    src/lowlevel_method.c
    src/native_bool.c
    src/native_capture.c
    src/native_int.c
    src/native_uint.c
    src/p6opaque.c
    src/smop.c
    src/smop_internal.h
    src/smop_interpreter.c
    src/smop_lowlevel.c
    src/smop_mold.c
    src/smop_s1p_array.c
    src/smop_s1p_attribute.c
    src/smop_s1p_capturize.c
    src/smop_s1p_ccode.c
    src/smop_s1p_code.c
    src/smop_s1p_defaultblocksignature.c
    src/smop_s1p_hash.c
    src/smop_s1p_io.c
    src/smop_s1p_lexicalscope.c
    src/smop_s1p_root_namespace.c
    src/smop_s1p_scalar.c
    src/smop_s1p_str.c
    src/smop_slime_capturize.c
    src/smop_slime_currentframe.c
    src/smop_slime_frame.c
    src/smop_slime_node.c
)

### tests
INCLUDE (FindThreads)
add_executable (01_smop_lowlevel.t test/01_smop_lowlevel.c)
target_link_libraries (01_smop_lowlevel.t ${CMAKE_THREAD_LIBS_INIT} smop)
