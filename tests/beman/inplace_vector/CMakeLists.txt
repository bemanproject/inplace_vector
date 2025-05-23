# cmake-format: off
# src/beman/inplace_vector/tests/CMakeLists.txt -*-makefile-*-
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
# cmake-format: on

# Tests
add_executable(beman.inplace_vector.test inplace_vector.test.cpp)
target_link_libraries(beman.inplace_vector.test PRIVATE beman.inplace_vector)
add_test(NAME beman.inplace_vector.test COMMAND beman.inplace_vector.test)

# Migrated test from original implementation
add_executable(beman.inplace_vector.ref-test ref_impl.test.cpp)
target_link_libraries(
    beman.inplace_vector.ref-test
    PRIVATE beman.inplace_vector
)
add_test(
    NAME beman.inplace_vector.ref-test
    COMMAND beman.inplace_vector.ref-test
)

# GoogleTest based tests
include(GoogleTest)

function(add_gtest NAME)
    add_executable(beman.inplace_vector.tests.${NAME})
    target_sources(beman.inplace_vector.tests.${NAME} PRIVATE ${NAME}.test.cpp)
    target_link_libraries(
        beman.inplace_vector.tests.${NAME}
        PRIVATE beman.inplace_vector GTest::gtest GTest::gtest_main
    )
    gtest_add_tests(beman.inplace_vector.tests.${NAME} "" AUTO)
endfunction()

# Tests for official specs
add_gtest(container_requirements)
add_gtest(triviality)
add_gtest(compare)
add_gtest(size_n_data)
add_gtest(erasure)
add_gtest(modifiers)

if(BEMAN_INPLACE_VECTOR_FREESTANDING_DELETED)
    add_gtest(freestanding)
else()
    add_gtest(container_requirements_fs)
    add_gtest(constructors_fs)
    add_gtest(size_n_data_fs)
    add_gtest(modifiers_fs)
endif()

# only add noexception tests if NO_EXCEPTIONS option is set and compiler supports -fno-exceptions
if(
    BEMAN_INPLACE_VECTOR_NO_EXCEPTIONS
    AND (NOT BEMAN_INPLACE_VECTOR_FREESTANDING_DELETED)
    AND (
        CMAKE_CXX_COMPILER_ID STREQUAL "Clang"
        OR CMAKE_CXX_COMPILER_ID STREQUAL "GNU"
    )
)
    add_gtest(noexceptions)
    target_compile_options(
        beman.inplace_vector.tests.noexceptions
        PRIVATE -fno-exceptions
    )
endif()

# constexpr test
add_executable(beman.inplace_vector.tests.constexpr constexpr.test.cpp)
target_link_libraries(
    beman.inplace_vector.tests.constexpr
    PRIVATE beman.inplace_vector
)
add_test(
    NAME beman.inplace_vector.tests.constexpr
    COMMAND beman.inplace_vector.tests.constexpr
)
