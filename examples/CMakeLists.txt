# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

set(ALL_EXAMPLES)

if(!BEMAN_INPLACE_VECTOR_FREESTANDING_DELETED)
    list(APPEND ALL_EXAMPLES fibonacci)
endif()

message("Examples to be built: ${ALL_EXAMPLES}")

foreach(example ${ALL_EXAMPLES})
    add_executable(beman.inplace_vector.examples.${example})
    target_sources(
        beman.inplace_vector.examples.${example}
        PRIVATE ${example}.cpp
    )
    target_link_libraries(
        beman.inplace_vector.examples.${example}
        beman.inplace_vector
    )
endforeach()
