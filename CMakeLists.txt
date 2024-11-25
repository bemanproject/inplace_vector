# cmake-format: off
# /CMakeLists.txt -*-makefile-*-
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
# cmake-format: on

set(CMAKE_SKIP_TEST_ALL_DEPENDENCY FALSE)

cmake_minimum_required(VERSION 3.25...3.31)

project(
    beman.inplace_vector
    VERSION 1.0.0
    DESCRIPTION
        "A dynamically-resizable vector with fixed capacity and embedded storage"
    LANGUAGES CXX
)

if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
    message(FATAL_ERROR "In-source builds are not allowed!")
endif()

# [CMAKE.SKIP_EXAMPLES]
option(
    BEMAN_EXEMPLAR_BUILD_EXAMPLES
    "Enable building examples. Default: ON. Values: { ON, OFF }."
    ${PROJECT_IS_TOP_LEVEL}
)

# [CMAKE.SKIP_TESTS]
option(
    BEMAN_INPLACE_VECTOR_BUILD_TESTS
    "Enable building tests and test infrastructure. Default: ON. Values: { ON, OFF }."
    ${PROJECT_IS_TOP_LEVEL}
)

set(CPACK_GENERATOR TGZ)

# includes
include(GNUInstallDirs)
include(CPack)

add_library(beman.inplace_vector INTERFACE)
# [CMAKE.LIBRARY_ALIAS]
add_library(beman::inplace_vector ALIAS beman.inplace_vector)

target_sources(
    beman.inplace_vector
    PUBLIC
        FILE_SET inplace_vector_public_headers
        TYPE HEADERS
        BASE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/include
        FILES
            "${CMAKE_CURRENT_SOURCE_DIR}/include/beman/inplace_vector/inplace_vector.hpp"
)
set_target_properties(
    beman.inplace_vector
    PROPERTIES VERIFY_INTERFACE_HEADER_SETS ON
)
target_compile_features(
    beman.inplace_vector
    INTERFACE
        "$<$<COMPILE_FEATURES:cxx_std_23>:cxx_std_23>"
        "$<$<NOT:$<COMPILE_FEATURES:cxx_std_23>>:cxx_std_20>"
)

set(TARGET_PACKAGE_NAME ${PROJECT_NAME}-config)
set(TARGETS_EXPORT_NAME ${PROJECT_NAME}-targets)
set(INSTALL_CONFIGDIR ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

# Install the InplaceVector library to the appropriate destination
install(
    TARGETS beman.inplace_vector
    EXPORT ${TARGETS_EXPORT_NAME}
    FILE_SET inplace_vector_public_headers
)

if(BEMAN_INPLACE_VECTOR_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests/beman/inplace_vector)
endif()

if(BEMAN_EXEMPLAR_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()
