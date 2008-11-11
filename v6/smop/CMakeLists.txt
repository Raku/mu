project (smop)
#SET( CMAKE_C_FLAGS "-O0 -g3 -DSMOP_LOWLEVEL_MEM_TRACE -DSMOP_HUNT_NULLS" )
SET( CMAKE_C_FLAGS "-O0 -g3 -DSMOP_LOWLEVEL_MEM_TRACE" )
#SET( CMAKE_C_FLAGS "-O0 -g3 -DSMOP_LOWLEVEL_MEM_TRACE -DSMOP_LOWLEVEL_MEM_DEBUG -DSMOP_MOLD_DEBUG -DSMOP_SLIME_DEBUG" )
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
        DEPENDS ${smop_SOURCE_DIR}/tools/ri
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
    src/lowlevel.sm0p
    src/s1p_defaultblocksignature.sm0p
    src/s1p_bindcapturesignature.sm0p
    src/s1p_pureprototypehow.sm0p
    src/p6opaque.sm0p
    src/s1p_code.sm0p
    src/s1p_map.sm0p
    src/s1p_grep.sm0p
    src/s1p_array_iterator.sm0p
)
compile_sm0p (${sm0p_file})
endforeach()

foreach (ri_file
    src/s1p_multi.ri
    src/s1p_io.ri
    src/s1p_package.ri
    src/s1p_lexicalscope.ri
    src/s1p_adhocsignature.ri
    src/s1p_attribute.ri
    src/s1p_hash_bvalue.ri
    src/native_int.ri
)
compile_ri (${ri_file})
endforeach()

INCLUDE (FindThreads)
add_library (smop SHARED
    src/idconst.c
    src/lowlevel_method.c
    src/native_bool.c
    src/native_capture.c
    src/native_int.c
    src/native_uint.c
    src/p6opaque.c
    src/smop.c
    src/haskell_ffi.c
    src/internal.h
    src/interpreter.c
    src/lowlevel.c
    src/mold.c
    src/s1p_array.c
    src/s1p_array_iterator.c
    src/s1p_attribute.c
    src/s1p_capturize.c
    src/s1p_ccode.c
    src/s1p_code.c
    src/s1p_grep.c
    src/s1p_map.c
    src/s1p_pureprototypehow.c
    src/s1p_emptylist.c
    src/s1p_endofiterator.c
    src/s1p_bindcapturesignature.c
    src/s1p_defaultblocksignature.c
    src/s1p_adhocsignature.c
    src/s1p_hash.c
    src/s1p_io.c
    src/s1p_multi.c
    src/s1p_package.c
    src/s1p_lexicalscope.c
    src/s1p_root_namespace.c
    src/s1p_lexical_prelude.c
    src/s1p_scalar.c
    src/s1p_str.c
    src/s1p_ritest.c
    src/s1p_hash_bvalue.c
    src/ri.c
    src/proto.c
#    src_s1p_itemcontext.c
#    src_s1p_itemrwcontext.c
)

file (MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/test)

### tests
INCLUDE (FindThreads)
foreach (test_file

    # SLIME tests
    #test/09_threads.sm0p
    #test/02_stack.sm0p
    #test/12_p6opaque.sm0p
    #test/16_labels.sm0p
    #test/17_sm0p.sm0p

    # v6-sm0p
    #test/14_p6opaque_methods.sm0p
    #test/22_smop_s1p_attribute.sm0p
    #test/28_pure_prototype_how.sm0p

    #test/01_smop_lowlevel.c

    test/03_const_identifier.c
    test/04_interpreter.c
    test/05_capture.c
    test/06_native_bool.c
    test/07_int.c
    test/07_native_int.c
    test/08_native_uint.c
    test/10_lowlevel_methods.c
    test/18_root_namespace.p6
    test/20_smop_s1p_ccode.c
    test/21_code.m0ld
    test/23_mold.sm0p
    test/24_m0ld.m0ld
    test/25_lexical_scope.p6
    test/27_default_block_signature.p6
    test/29_bind_capture_signature.p6
    test/30_array_map.m0ld
    test/31_int_postfix_plusplus.m0ld
    test/32_array_iterator.m0ld
    # test/33_pugs_simple.p6-pugs # would require building pugs twice
    test/34_array_unshift.m0ld
    test/35_array_grep.m0ld
    test/36_adhoc_signature.m0ld
    test/37_multisub.m0ld
    test/38_ritest.m0ld
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
