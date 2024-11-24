# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

cmake_minimum_required(VERSION 3.25...3.31)

project(beman_inplace_vector_example LANGUAGES CXX)

if(PROJECT_IS_TOP_LEVEL)
    find_package(beman_inplace_vector 1.0.0 EXACT REQUIRED)
    enable_testing()
endif()

set(ALL_EXAMPLES fibonacci)

message(STATUS "Examples to be built: ${ALL_EXAMPLES}")

foreach(example ${ALL_EXAMPLES})
    add_executable(beman.inplace_vector.examples.${example})
    target_sources(
        beman.inplace_vector.examples.${example}
        PRIVATE ${example}.cpp
    )
    target_link_libraries(
        beman.inplace_vector.examples.${example}
        beman::beman_inplace_vector
    )
    add_test(NAME ${example} COMMAND beman.inplace_vector.examples.${example})
endforeach()
