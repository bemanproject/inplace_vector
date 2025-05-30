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
    "Enable building examples. Default: ${PROJECT_IS_TOP_LEVEL}. Values: { ON, OFF}."
    ${PROJECT_IS_TOP_LEVEL}
)

# [CMAKE.SKIP_TESTS]
option(
    BEMAN_INPLACE_VECTOR_BUILD_TESTS
    "Enable building tests and test infrastructure. Default: ${PROJECT_IS_TOP_LEVEL}. Values: { ON, OFF }."
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
include(CMakePackageConfigHelpers)

add_library(${PROJECT_NAME} INTERFACE)
# [CMAKE.LIBRARY_ALIAS]
add_library(beman::inplace_vector ALIAS ${PROJECT_NAME})

target_sources(
    ${PROJECT_NAME}
    INTERFACE
        FILE_SET HEADERS
        BASE_DIRS
            ${CMAKE_CURRENT_SOURCE_DIR}/include
            ${CMAKE_CURRENT_BINARY_DIR}/include
        FILES
            ${CMAKE_CURRENT_SOURCE_DIR}/include/beman/inplace_vector/inplace_vector.hpp
            ${CMAKE_CURRENT_BINARY_DIR}/include/beman/inplace_vector/config.hpp
)

# Install the INTERFACE library target and create an export set
install(TARGETS ${PROJECT_NAME} EXPORT ${PROJECT_NAME}-targets FILE_SET HEADERS)

configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/${PROJECT_NAME}-config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}
)

write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

install(
    FILES
        "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}
)

install(
    EXPORT ${PROJECT_NAME}-targets
    FILE ${PROJECT_NAME}-targets.cmake
    NAMESPACE beman::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}
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
