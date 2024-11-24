# cmake-format: off
# src/beman/inplace_vector/tests/CMakeLists.txt -*-makefile-*-
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
# cmake-format: on

# Tests
add_executable(beman.inplace_vector.test inplace_vector.test.cpp)

target_link_libraries(
    beman.inplace_vector.test
    PRIVATE beman::beman_inplace_vector
)

add_test(NAME beman.inplace_vector.test COMMAND beman.inplace_vector.test)

# test if the targets are findable from the build directory
# NOTE: For an INTERFACE library and multi config generators, we may always use
# the -C Debug config type instead of  $<CONFIG> to prevent problems! CK
if(CMAKE_BUILD_TYPE STREQUAL Debug)
    add_test(
        NAME find-package-test
        COMMAND
            ${CMAKE_CTEST_COMMAND} --verbose --output-on-failure -C Debug
            --build-and-test "${PROJECT_SOURCE_DIR}/examples"
            "${CMAKE_CURRENT_BINARY_DIR}/find-package-test" --build-generator
            ${CMAKE_GENERATOR} --build-makeprogram ${CMAKE_MAKE_PROGRAM}
            --build-options "-DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}"
            "-DCMAKE_CXX_STANDARD=${CMAKE_CXX_STANDARD}"
            "-DCMAKE_BUILD_TYPE=Debug"
            "-DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH}"
    )
endif()
