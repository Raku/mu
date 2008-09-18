project (smop)
SET( CMAKE_C_FLAGS "-O0 -g3 -DSMOP_LOWLEVEL_MEM_DEBUG -DSMOP_LOWLEVEL_MEM_TRACE" )
SET( CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS TRUE )
cmake_minimum_required (VERSION 2.6)
include_directories (${smop_SOURCE_DIR}/include)
find_program (CABAL cabal ~/.cabal/bin)

if(NOT CABAL)
    message(FATAL "ERROR cabal is not found, please install cabal-install, you can find it in third-party/cabal-install in the pugs repository or at http://hackage.haskell.org/cgi-bin/hackage-scripts/package/cabal-install")
endif(NOT CABAL)

### m0ld

    # TODO remove use of copy
    add_custom_command (
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe
        WORKING_DIRECTORY ${smop_SOURCE_DIR}/m0ld
        COMMAND ${CABAL} ARGS configure --distpref=${CMAKE_CURRENT_BINARY_DIR}/cabal_dist
        COMMAND ${CABAL} ARGS build --distpref=${CMAKE_CURRENT_BINARY_DIR}/cabal_dist
        COMMAND cp ARGS ${CMAKE_CURRENT_BINARY_DIR}/cabal_dist/build/m0ld/m0ld ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe
        DEPENDS ${smop_SOURCE_DIR}/m0ld/M0ld.hs
    )

### DSL 
### TODO refactor

file (MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src)

macro (compile_sm0p sm0p_file)
    string(REGEX REPLACE ".sm0p$" ".c" c_file ${sm0p_file})
    add_custom_command (
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        COMMAND perl ARGS ${smop_SOURCE_DIR}/sm0p.pl ${smop_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe ${smop_SOURCE_DIR}/${sm0p_file} ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe
        DEPENDS ${smop_SOURCE_DIR}/sm0p.pl
        DEPENDS ${smop_SOURCE_DIR}/${sm0p_file}
    )
endmacro(compile_sm0p)

macro (compile_m0ld dsl_file)
    string(REGEX REPLACE ".m0ld$" ".c" c_file ${dsl_file})
    add_custom_command (
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        COMMAND perl ARGS ${smop_SOURCE_DIR}/tools/dsl m0ld ${smop_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe ${smop_SOURCE_DIR}/${dsl_file} ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe
        DEPENDS ${smop_SOURCE_DIR}/${dsl_file}
    )
endmacro(compile_m0ld)

macro (compile_ri dsl_file)
    string(REGEX REPLACE ".ri$" ".c" c_file ${dsl_file})
    add_custom_command (
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        COMMAND perl ARGS ${smop_SOURCE_DIR}/tools/dsl ri ${smop_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe ${smop_SOURCE_DIR}/${dsl_file} ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        DEPENDS ${smop_SOURCE_DIR}/${dsl_file}
    )
endmacro(compile_ri)

macro (compile_perl6 dsl_file)
    string(REGEX REPLACE ".p6$" ".m0ld" m0ld_file ${dsl_file})
    string(REGEX REPLACE ".m0ld$" ".c" c_file ${m0ld_file})
    add_custom_command (
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${m0ld_file}
        COMMAND perl ARGS ${smop_SOURCE_DIR}/tools/dsl p6 ${smop_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe ${smop_SOURCE_DIR}/${dsl_file} ${CMAKE_CURRENT_BINARY_DIR}/${m0ld_file}
        COMMAND perl ARGS ${smop_SOURCE_DIR}/tools/dsl m0ld ${smop_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe ${CMAKE_CURRENT_BINARY_DIR}/${m0ld_file} ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe
        DEPENDS ${smop_SOURCE_DIR}/${dsl_file}
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/../../misc/elfish/elfX/elfX
    )
endmacro(compile_perl6)

macro (compile_perl6_pugs dsl_file)
    string(REGEX REPLACE ".p6-pugs$" ".m0ld" m0ld_file ${dsl_file})
    string(REGEX REPLACE ".m0ld$" ".c" c_file ${m0ld_file})
    add_custom_command (
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${m0ld_file}
        COMMAND perl ARGS ${smop_SOURCE_DIR}/tools/dsl p6-pugs ${smop_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe ${smop_SOURCE_DIR}/${dsl_file} ${CMAKE_CURRENT_BINARY_DIR}/${m0ld_file}
        COMMAND perl ARGS ${smop_SOURCE_DIR}/tools/dsl m0ld ${smop_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe ${CMAKE_CURRENT_BINARY_DIR}/${m0ld_file} ${CMAKE_CURRENT_BINARY_DIR}/${c_file}
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/m0ld_exe
        DEPENDS ${smop_SOURCE_DIR}/${dsl_file}
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/../../pugs
    )
endmacro(compile_perl6_pugs)

foreach (sm0p_file
    src/smop_lowlevel.sm0p
    src/smop_s1p_lexicalscope.sm0p
    src/smop_s1p_defaultblocksignature.sm0p
    src/smop_s1p_bindcapturesignature.sm0p
    src/smop_s1p_pureprototypehow.sm0p
    src/p6opaque.sm0p
    src/smop_s1p_attribute.sm0p
    src/smop_s1p_code.sm0p
    src/smop_s1p_map.sm0p
    src/smop_s1p_array_iterator.sm0p
)
compile_sm0p (${sm0p_file})
endforeach()

foreach (ri_file
    src/smop_s1p_io.ri
)
compile_ri (${ri_file})
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
    src/smop_haskell_ffi.c
    src/smop_internal.h
    src/smop_interpreter.c
    src/smop_lowlevel.c
    src/smop_mold.c
    src/smop_s1p_array.c
    src/smop_s1p_array_iterator.c
    src/smop_s1p_attribute.c
    src/smop_s1p_capturize.c
    src/smop_s1p_ccode.c
    src/smop_s1p_code.c
    src/smop_s1p_map.c
    src/smop_s1p_pureprototypehow.c
    src/smop_s1p_emptylist.c
    src/smop_s1p_bindcapturesignature.c
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
    src/smop_ri.c
#    src/smop_s1p_itemcontext.c
#    src/smop_s1p_itemrwcontext.c
)

file (MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/test)

### tests
INCLUDE (FindThreads)
foreach (test_file
    test/01_smop_lowlevel.c
    test/02_stack.sm0p
    test/03_const_identifier.c
    test/04_interpreter.c
    test/05_capture.c
    test/06_native_bool.c
    test/07_int.c
    test/07_native_int.c
    test/08_native_uint.c
    test/09_threads.sm0p
    test/10_lowlevel_methods.c
    test/11_lowlevel_scalar.sm0p
    test/12_p6opaque.sm0p
    test/14_p6opaque_methods.sm0p
    test/15_hash.p6
    test/16_labels.sm0p
    test/17_sm0p.sm0p
    test/18_root_namespace.p6
    test/19_array.p6
    test/20_smop_s1p_ccode.c
    test/21_code.m0ld
    test/22_smop_s1p_attribute.sm0p
    test/23_mold.sm0p
    test/24_m0ld.m0ld
    test/25_lexical_scope.p6
    test/27_default_block_signature.p6
    test/28_pure_prototype_how.sm0p
    test/29_bind_capture_signature.p6
    test/30_array_map.m0ld
    test/31_int_postfix_plusplus.m0ld
    test/32_array_iterator.m0ld
    test/33_pugs_simple.p6-pugs
)
    set(c_file)
    if (test_file MATCHES "\\.sm0p$")
        compile_sm0p (${test_file})
        string(REGEX REPLACE ".sm0p$" ".c" c_file ${test_file})
    elseif (test_file MATCHES "\\.m0ld$")
        compile_m0ld (${test_file})
        string(REGEX REPLACE ".m0ld$" ".c" c_file ${test_file})
    elseif (test_file MATCHES "\\.p6$")
        compile_perl6 (${test_file})
        string(REGEX REPLACE "\\.p6$" ".c" c_file ${test_file})
    elseif (test_file MATCHES "\\.p6-pugs$")
        compile_perl6_pugs (${test_file})
        string(REGEX REPLACE "\\.p6-pugs$" ".c" c_file ${test_file})
    elseif (test_file MATCHES "\\.c$")
        set(c_file ${test_file})
    endif (test_file MATCHES "\\.sm0p$")

    if (c_file)
        string(REGEX REPLACE ".c$" ".t" out ${c_file})
        add_executable (${out} ${c_file})
        target_link_libraries (${out} ${CMAKE_THREAD_LIBS_INIT} smop)
    endif()
endforeach()
