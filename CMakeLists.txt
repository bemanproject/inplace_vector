# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

cmake_minimum_required(VERSION 3.30...4.3)

project(
    beman.inplace_vector
    DESCRIPTION
        "A dynamically-resizable vector with fixed capacity and embedded storage"
    LANGUAGES CXX
    VERSION 0.1.0
)

# [CMAKE.SKIP_TESTS]
option(
    BEMAN_INPLACE_VECTOR_BUILD_TESTS
    "Enable building tests and test infrastructure. Default: ${PROJECT_IS_TOP_LEVEL}. Values: { ON, OFF }."
    ${PROJECT_IS_TOP_LEVEL}
)

# [CMAKE.SKIP_EXAMPLES]
option(
    BEMAN_INPLACE_VECTOR_BUILD_EXAMPLES
    "Enable building examples. Default: ${PROJECT_IS_TOP_LEVEL}. Values: { ON, OFF }."
    ${PROJECT_IS_TOP_LEVEL}
)

option(
    BEMAN_INPLACE_VECTOR_NO_EXCEPTIONS
    "Disable exceptions by replacing throw calls with abort. Default: OFF. Values: { ON, OFF }."
    OFF
)

configure_file(
    "${PROJECT_SOURCE_DIR}/include/beman/inplace_vector/config_generated.hpp.in"
    "${PROJECT_BINARY_DIR}/include/beman/inplace_vector/config_generated.hpp"
    @ONLY
)

# for find of beman_install_library and configure_build_telemetry
include(infra/cmake/beman-install-library.cmake)
include(infra/cmake/BuildTelemetryConfig.cmake)

add_library(beman.inplace_vector INTERFACE)
add_library(beman::inplace_vector ALIAS beman.inplace_vector)

target_sources(
    beman.inplace_vector
    PUBLIC
        FILE_SET HEADERS
            BASE_DIRS
                "${CMAKE_CURRENT_SOURCE_DIR}/include"
                "${CMAKE_CURRENT_BINARY_DIR}/include"
)

set_target_properties(
    beman.inplace_vector
    PROPERTIES VERIFY_INTERFACE_HEADER_SETS ${PROJECT_IS_TOP_LEVEL}
)

add_subdirectory(include/beman/inplace_vector)

beman_install_library(beman.inplace_vector TARGETS beman.inplace_vector)
configure_build_telemetry()

if(BEMAN_INPLACE_VECTOR_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests/beman/inplace_vector)
endif()

if(BEMAN_INPLACE_VECTOR_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()
