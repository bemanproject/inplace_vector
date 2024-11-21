# cmake-format: off
# src/beman/inplace_vector/tests/CMakeLists.txt -*-makefile-*-
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
# cmake-format: on

# Tests
add_executable(beman.inplace_vector.test inplace_vector.test.cpp)

target_link_libraries(beman.inplace_vector.test PRIVATE beman.inplace_vector)

add_test(NAME beman.inplace_vector.test COMMAND beman.inplace_vector.test)

if(CMAKE_CXX_STANDARD GREATER_EQUAL 20)
    # constexpr test
    add_executable(beman.inplace_vector.constexpr_test constexpr.test.cpp)
    target_link_libraries(
        beman.inplace_vector.constexpr_test
        PRIVATE beman.inplace_vector
    )
    add_test(
        NAME beman.inplace_vector.constexpr_test
        COMMAND beman.inplace_vector.constexpr_test
    )
else()
    message(WARNING "C++20 or later is not enabled, skipping constexpr test.")
endif()
