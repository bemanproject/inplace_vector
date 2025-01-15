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

add_executable(beman.inplace_vector.tests.spec)
target_sources(beman.inplace_vector.tests.spec PRIVATE spec.test.cpp)
target_link_libraries(
    beman.inplace_vector.tests.spec
    PRIVATE beman.inplace_vector GTest::gtest GTest::gtest_main
)

gtest_add_tests(beman.inplace_vector.tests.spec "" AUTO)
