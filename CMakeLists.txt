# cmake-format: off
# /CMakeLists.txt -*-makefile-*-
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
# cmake-format: on

cmake_minimum_required(VERSION 3.23)

project(
    beman.inplace_vector
    VERSION 1.0.0
    DESCRIPTION
        "A dynamically-resizable vector with fixed capacity and embedded storage"
    LANGUAGES CXX
)

# [CMAKE.SKIP_EXAMPLES]
option(
    BEMAN_INPLACE_VECTOR_BUILD_EXAMPLES
    "Enable building examples. Default: ON. Values: { ON, OFF }."
    ${PROJECT_IS_TOP_LEVEL}
)

# [CMAKE.SKIP_TESTS]
option(
    BEMAN_INPLACE_VECTOR_BUILD_TESTS
    "Enable building tests and test infrastructure. Default: ON. Values: { ON, OFF }."
    ${PROJECT_IS_TOP_LEVEL}
)

option(
    BEMAN_INPLACE_VECTOR_NO_EXCEPTIONS
    "Disable exceptions by replacing throw calls with abort. Default: OFF. Values: { ON, OFF }."
    OFF
)

configure_file(
    "${PROJECT_SOURCE_DIR}/include/beman/inplace_vector/config.hpp.in"
    "${PROJECT_BINARY_DIR}/include/beman/inplace_vector/config.hpp"
    @ONLY
)

include(FetchContent)
include(GNUInstallDirs)

add_library(beman.inplace_vector INTERFACE)
# [CMAKE.LIBRARY_ALIAS]
add_library(beman::inplace_vector ALIAS beman.inplace_vector)

target_include_directories(
    beman.inplace_vector
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

# Install the InplaceVector library to the appropriate destination
install(
    TARGETS beman.inplace_vector
    EXPORT beman.inplace_vector-targets
    FILE_SET HEADERS
)

# Install the header files to the appropriate destination
install(
    DIRECTORY
        ${CMAKE_CURRENT_SOURCE_DIR}/include/
        ${CMAKE_CURRENT_BINARY_DIR}/include/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    FILES_MATCHING
    PATTERN "*.hpp"
)

if(BEMAN_INPLACE_VECTOR_BUILD_TESTS)
    include(CTest)
    enable_testing()

    # Fetch GoogleTest
    FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG
            f8d7d77c06936315286eb55f8de22cd23c188571 # release-1.14.0
        EXCLUDE_FROM_ALL
    )

    block()
        set(INSTALL_GTEST OFF) # Disable GoogleTest installation
        FetchContent_MakeAvailable(googletest)
    endblock()

    add_subdirectory(tests/beman/inplace_vector)
endif()

if(BEMAN_INPLACE_VECTOR_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()
