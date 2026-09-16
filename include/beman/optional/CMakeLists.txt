# include/beman/optional/CMakeLists.txt                             -*-cmake-*-
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

target_sources(
    beman.optional
    PUBLIC
        FILE_SET beman_optional_headers
            TYPE HEADERS
            FILES
                optional.hpp
                detail/iterator.hpp
                detail/stl_interfaces/config.hpp
                detail/stl_interfaces/fwd.hpp
                detail/stl_interfaces/iterator_interface.hpp
)
