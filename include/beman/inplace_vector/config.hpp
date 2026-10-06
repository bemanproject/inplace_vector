// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef BEMAN_INPLACE_VECTOR_CONFIG_HPP
#define BEMAN_INPLACE_VECTOR_CONFIG_HPP

#if !defined(__has_include) ||                                                 \
    __has_include(<beman/inplace_vector/config_generated.hpp>)
#include <beman/inplace_vector/config_generated.hpp>
#else
#define BEMAN_INPLACE_VECTOR_NO_EXCEPTIONS() 0
#endif

#ifndef BEMAN_INPLACE_VECTOR_HAS_TRIVIAL_UNION
#if defined(__cpp_trivial_union) && __cpp_trivial_union >= 202602L
#define BEMAN_INPLACE_VECTOR_HAS_TRIVIAL_UNION 1
#else
#define BEMAN_INPLACE_VECTOR_HAS_TRIVIAL_UNION 0
#endif
#endif

#endif
