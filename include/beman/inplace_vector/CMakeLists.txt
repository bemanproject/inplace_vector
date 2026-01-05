# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

target_sources(
    beman.inplace_vector
    PUBLIC FILE_SET HEADERS
    FILES
        inplace_vector.hpp
        "${CMAKE_CURRENT_BINARY_DIR}/config.hpp"
)
